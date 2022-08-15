use std::collections::HashMap;
use std::collections::HashSet;
use std::io::BufWriter;
use std::io::Write;
use std::sync::Condvar;
use std::sync::{Arc, Mutex};
use std::thread;

use crate::ast::*;
use crate::memory::*;
use crate::parser::*;
use crate::util::logical_cpu_count;

struct CodeGenPool {
    running_fibers: usize,
    functions: Vec<RSFunction>,
    // Function name -> instructions
    results: Vec<(String, Vec<String>)>,
    errors: Vec<CompErr>,
}

#[derive(Debug, Clone)]
enum ScopeEntry {
    // Contains the number of args
    Fun(usize),
    Var(Loc),
    // First parameter are the macro args
    Define(Vec<String>, Expr),
}

struct FunContext<'a> {
    global_scope: &'a HashMap<String, ScopeEntry>,
    /*
     * One HashMap for the entire function scope, for O(1) access
     * I thought about using a linked list of hashmaps...
     * But that's a lot of overhead
     */
    fun_scope: HashMap<String, ScopeEntry>,
    // Keeps track of the vars dedicated to the current block
    // So at the end of a block scope we can remove from the fun_scope
    block_vars: Vec<Vec<String>>,
    local_var_locs: HashMap<String, Loc>,
    // Maps from visible label name to compiled label name
    labels: HashMap<String, String>,
    // So we never run out of unique labels
    func_id: usize,
    label_counter: usize,
    // Stack of end labels to tell break where to jump to
    break_dest_stack: Vec<String>,
}

impl FunContext<'_> {
    fn new_label(&mut self, prefix: &str) -> String {
        self.label_counter += 1;
        // By prefixing with '.', it guarantees no collisions with user labels
        format!(".{}_{}_{}", prefix, self.func_id, self.label_counter)
    }

    fn new_scope(&mut self) {
        self.block_vars.push(vec![]);
    }

    fn drop_scope(&mut self) {
        // We know it must exist, since a scope is pushed on function entry
        for var in self.block_vars.pop().unwrap() {
            self.fun_scope.remove(&var);
        }
    }

    fn add_to_scope(&mut self, pos: &Pos, name: String, entry: ScopeEntry) -> Result<(), CompErr> {
        if self.fun_scope.contains_key(&name) {
            return CompErr::err(pos, format!("{} is already in defined in this scope", name));
        }
        self.fun_scope.insert(name.clone(), entry);
        self.block_vars.last_mut().unwrap().push(name);
        Ok(())
    }

    fn find_in_scope(&self, name: &str) -> Option<&ScopeEntry> {
        if let Some(entry) = self.fun_scope.get(name) {
            return Some(entry);
        }
        match self.global_scope.get(name) {
            // Only allow referencing global vars when users specify "extrn"
            Some(ScopeEntry::Var(Loc::Data(_))) => None,
            other => other,
        }
    }
}

fn label_for_string_id(file_id: usize, string_index: usize) -> String {
    format!(".STR_{}_{}", file_id, string_index)
}

/**
 * Allocates stack memory for auto local_var_locs and finds labels.
 * @param body The function body to search for auto declarations
 * @param offset The positive offset from rbp (how much space came before this)
 */
fn prepass_gen(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    body: &Statement,
    offset: i64,
) -> Result<(), CompErr> {
    let mut stack = vec![body];
    let mut autos_size = 0;

    // DFS to find them all
    while !stack.is_empty() {
        match stack.pop().unwrap() {
            Statement::Label(pos, name) => {
                if c.labels.contains_key(name) {
                    return CompErr::err(
                        pos,
                        format!("Label {} already defined in this function", name),
                    );
                }

                let l = c.new_label(&format!("LAB_{}", name).to_string());
                c.labels.insert(name.clone(), l);
            }
            Statement::Auto(pos, vars) => {
                for var in vars {
                    let name = var.name();

                    if c.local_var_locs.contains_key(name) {
                        return CompErr::err(
                            pos,
                            format!("{} already defined in this function", name),
                        );
                    }

                    let size = match var {
                        Var::Vec(_, vec_size, _) => 1 + vec_size,
                        Var::Single(_, _) => 1,
                    };

                    c.local_var_locs
                        .insert(name.clone(), Loc::Stack(-offset - autos_size));
                    autos_size += size;
                }
            }
            Statement::Block(statements) => {
                for s in statements {
                    stack.push(s);
                }
            }
            Statement::If(_, if_body, Some(else_body)) => {
                stack.push(if_body);
                stack.push(else_body);
            }
            Statement::If(_, body, None) => stack.push(body),
            Statement::While(_, body) => stack.push(body),
            _ => {}
        }
    }

    if autos_size > 0 {
        instructions.push(format!("subq ${}, %rsp", 8 * autos_size));
    }

    Ok(())
}

// Allocates the necessary args on the stack
fn alloc_args(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    pos: &Pos,
    args: &[String],
) -> Result<(), CompErr> {
    for (i, arg) in args.iter().enumerate() {
        let loc = if i < 6 {
            let register = Reg::for_arg_num(i);
            instructions.push(format!("pushq %{}", register));
            Loc::Stack(-(i as i64) - 1)
        } else {
            Loc::Stack((i as i64) - 4)
        };

        c.local_var_locs.insert(arg.clone(), loc.clone());
        c.add_to_scope(pos, arg.clone(), ScopeEntry::Var(loc))?;
    }
    Ok(())
}

fn gen_op_cmp(
    instructions: &mut Vec<String>,
    command: &str,
    lhs_loc: Loc,
    rhs_loc: Loc,
) -> (Loc, RegSet) {
    instructions.push(format!("cmpq {},{}", rhs_loc, lhs_loc));
    instructions.push(format!("movq $0,{}", lhs_loc));

    if let Loc::Register(lhs_reg) = lhs_loc {
        instructions.push(format!("{} %{}", command, lhs_reg.low_byte()));
    } else {
        panic!("LHS must be a register");
    }

    (lhs_loc, RegSet::empty())
}

fn gen_op_single(
    instructions: &mut Vec<String>,
    command: &str,
    lhs_loc: Loc,
    rhs_loc: Loc,
) -> (Loc, RegSet) {
    instructions.push(format!("{} {},{}", command, rhs_loc, lhs_loc));
    (lhs_loc, RegSet::empty())
}

fn gen_op_shift(
    instructions: &mut Vec<String>,
    command: &str,
    lhs_loc: Loc,
    rhs_loc: Loc,
) -> (Loc, RegSet) {
    match rhs_loc {
        rhs_loc @ Loc::Immediate(_) => gen_op_single(instructions, command, lhs_loc, rhs_loc),
        _ => {
            // Required to use %cl register for non immediates during shifts
            instructions.push(format!("movq {},%rcx", rhs_loc));
            instructions.push(format!("{} %cl,{}", command, lhs_loc));
            let used_registers = RegSet::of(Reg::Rcx);
            (lhs_loc, used_registers)
        }
    }
}

/**
 * Prepares sending an LHS and RHS to the FPU
 * @return (instructions, rhs_loc, used_registers)
 */
fn gen_op_pre_float_op(
    instructions: &mut Vec<String>,
    lhs_loc: Loc,
    init_rhs_loc: Loc,
) -> (Loc, RegSet) {
    // rax and rdx are always used for div or mod
    let mut used_registers = RegSet::of(Reg::Rax).with(Reg::Rdx);

    let should_move_rhs = matches!(
        init_rhs_loc,
        Loc::Register(Reg::Rax) | Loc::Register(Reg::Rdx) | Loc::Immediate(_)
    );

    // Move rhs to a new register if necessary
    let rhs_loc = if should_move_rhs {
        // Make sure we don't override LHS
        let dest_reg = match lhs_loc {
            Loc::Register(Reg::Rcx) => Reg::Rdi,
            _ => Reg::Rcx,
        };

        used_registers = used_registers.with(dest_reg);
        instructions.push(format!("movq {},%{}", init_rhs_loc, dest_reg));

        Loc::Register(dest_reg)
    } else {
        init_rhs_loc
    };

    match lhs_loc {
        Loc::Register(Reg::Rax) => {}
        lhs_loc => instructions.push(format!("movq {},%rax", lhs_loc)),
    };

    (rhs_loc, used_registers)
}

/**
 * Generate instructions to diving (or mod) two numbers
 * As per x86 idivq, the results always go to rax and rdx
 */
fn gen_op_pre_div(instructions: &mut Vec<String>, lhs_loc: Loc, init_rhs_loc: Loc) -> RegSet {
    let (rhs_loc, used_registers) = gen_op_pre_float_op(instructions, lhs_loc, init_rhs_loc);

    instructions.push("movq $0,%rdx".to_string());
    instructions.push(format!("idivq {}", rhs_loc));

    used_registers
}

fn gen_op_mod(instructions: &mut Vec<String>, lhs_loc: Loc, rhs_loc: Loc) -> (Loc, RegSet) {
    let used_registers = gen_op_pre_div(instructions, lhs_loc, rhs_loc);
    (Loc::Register(Reg::Rdx), used_registers)
}

fn gen_op_div(instructions: &mut Vec<String>, lhs_loc: Loc, rhs_loc: Loc) -> (Loc, RegSet) {
    let used_registers = gen_op_pre_div(instructions, lhs_loc, rhs_loc);
    (Loc::Register(Reg::Rax), used_registers)
}

fn gen_op_mul(instructions: &mut Vec<String>, lhs_loc: Loc, rhs_loc: Loc) -> (Loc, RegSet) {
    let (rhs_loc, used_registers) = gen_op_pre_float_op(instructions, lhs_loc, rhs_loc);
    instructions.push(format!("imulq {}", rhs_loc));
    (Loc::Register(Reg::Rax), used_registers)
}

/**
 * Generates instructions for applying the operator to the given lhs & rhs.
 * May override either lhs_loc or rhs_loc if they are registers.
 * ASSUMES lhs_loc is already in a register
 * @return (instructions, dest_loc, extra_registers)
 */
fn gen_op_command(
    instructions: &mut Vec<String>,
    op: &BinOp,
    lhs_loc: Loc,
    rhs_loc: Loc,
) -> (Loc, RegSet) {
    match op {
        BinOp::Add => gen_op_single(instructions, "addq", lhs_loc, rhs_loc),
        BinOp::Sub => gen_op_single(instructions, "subq", lhs_loc, rhs_loc),
        BinOp::Mod => gen_op_mod(instructions, lhs_loc, rhs_loc),
        BinOp::Div => gen_op_div(instructions, lhs_loc, rhs_loc),
        BinOp::Mul => gen_op_mul(instructions, lhs_loc, rhs_loc),
        BinOp::ShiftRight => gen_op_shift(instructions, "shrq", lhs_loc, rhs_loc),
        BinOp::ShiftLeft => gen_op_shift(instructions, "shlq", lhs_loc, rhs_loc),
        BinOp::And => gen_op_single(instructions, "andq", lhs_loc, rhs_loc),
        BinOp::Or => gen_op_single(instructions, "orq", lhs_loc, rhs_loc),
        BinOp::Xor => gen_op_single(instructions, "xorq", lhs_loc, rhs_loc),
        BinOp::Eq => gen_op_cmp(instructions, "sete", lhs_loc, rhs_loc),
        BinOp::Ne => gen_op_cmp(instructions, "setne", lhs_loc, rhs_loc),
        BinOp::Le => gen_op_cmp(instructions, "setle", lhs_loc, rhs_loc),
        BinOp::Lt => gen_op_cmp(instructions, "setl", lhs_loc, rhs_loc),
        BinOp::Ge => gen_op_cmp(instructions, "setge", lhs_loc, rhs_loc),
        BinOp::Gt => gen_op_cmp(instructions, "setg", lhs_loc, rhs_loc),
        BinOp::Assign(_) => panic!("Assignments should not be parsed as regular binop exprs"),
    }
}

/// Returns the registers that the given op MIGHT use
fn registers_for_op(op: &BinOp) -> RegSet {
    let fpu_regset = RegSet::of(Reg::Rax).with(Reg::Rdx);
    let shift_regset = RegSet::of(Reg::Rcx);

    match op {
        BinOp::ShiftRight => shift_regset,
        BinOp::ShiftLeft => shift_regset,
        BinOp::Mod => fpu_regset,
        BinOp::Div => fpu_regset,
        BinOp::Mul => fpu_regset,
        _ => RegSet::empty(),
    }
}

fn get_safe_registers(used_registers: RegSet) -> RegSet {
    RegSet::usable_caller_save().subtract(&used_registers)
}

/// Plans evaluation strategy for an LHS and RHS expression.
/// It will try to store the LHS value in a register if possible.
/// Otherwise it stores to the stack.
/// The returned `lhs_loc` is guaranteed to be in a register
/// Return format: `(instructions, lhs_loc, rhs_loc, used_registers)`
/// #Arguments
/// * `unsafe_registers` - Registers which should be avoided
fn gen_pre_op(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    lhs: &Expr,
    rhs: &Expr,
    unsafe_registers: RegSet,
) -> Result<(Loc, Loc, RegSet), CompErr> {
    // Generate instructions for RHS first so we know which registers are safe
    let mut rhs_ins = vec![];
    let (rhs_loc, mut used_registers) = gen_expr(c, &mut rhs_ins, rhs)?;

    let safe_registers = get_safe_registers(used_registers.clone()).subtract(&unsafe_registers);

    let (lhs_loc, lhs_registers) = gen_expr(c, instructions, lhs)?;
    used_registers = used_registers.union(lhs_registers);

    // If the lhs_loc is already in a safe register, don't move it
    if let Loc::Register(lhs_reg) = lhs_loc {
        if safe_registers.contains(lhs_reg) {
            instructions.append(&mut rhs_ins);
            return Ok((lhs_loc, rhs_loc, used_registers));
        }
    }

    let new_lhs_loc = match safe_registers.first() {
        // If there are safe registers available, store the lhs there
        Some(dest_reg) => {
            used_registers = used_registers.with(dest_reg);
            instructions.push(format!("movq {},%{}", lhs_loc, dest_reg));
            instructions.append(&mut rhs_ins);
            Loc::Register(dest_reg)
        }
        // Nowhere is safe! Store LHS on the stack
        None => {
            let lhs_in_reg = matches!(lhs_loc, Loc::Register(_));

            if lhs_in_reg {
                instructions.push(format!("pushq {}", lhs_loc));
            }

            instructions.append(&mut rhs_ins);

            // Don't need to update used_registers because...
            // we already know everything is used!
            let new_lhs_loc = match rhs_loc {
                Loc::Register(Reg::Rax) => Loc::Register(Reg::Rcx),
                _ => Loc::Register(Reg::Rax),
            };

            if lhs_in_reg {
                instructions.push(format!("popq {}", new_lhs_loc));
            } else {
                instructions.push(format!("movq {},{}", lhs_loc, new_lhs_loc));
            }
            new_lhs_loc
        }
    };

    Ok((new_lhs_loc, rhs_loc, used_registers))
}

fn gen_prep_unary_op_incdec(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (expr_loc, used_registers) = gen_expr(c, instructions, expr)?;

    match expr_loc {
        Loc::Register(_) | Loc::Immediate(_) => {
            return CompErr::err(
                &expr.pos(),
                "`++` or `--` must operate on a memory location".to_string(),
            )
        }
        _ => {}
    };
    Ok((expr_loc, used_registers))
}

fn gen_unary_op_pre_incdec(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    op_name: &str,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (expr_loc, used_registers) = gen_prep_unary_op_incdec(c, instructions, expr)?;
    instructions.push(format!("{} {}", op_name, expr_loc));
    Ok((expr_loc, used_registers))
}

fn gen_unary_op_post_incdec(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    op_name: &str,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (expr_loc, used_registers) = gen_prep_unary_op_incdec(c, instructions, expr)?;

    // We can pick any register since we know expr_loc MUST NOT be in a register
    let dest_reg = Reg::R11;
    let dest_loc = Loc::Register(dest_reg);

    instructions.push(format!("movq {},{}", expr_loc, dest_loc));
    instructions.push(format!("{} {}", op_name, expr_loc));
    Ok((dest_loc, used_registers.with(dest_reg)))
}

fn gen_unary_op_non_assign(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    asm_op: &str,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (expr_loc, mut used_registers) = gen_expr(c, instructions, expr)?;
    let dest_reg = match expr_loc {
        Loc::Register(reg) => reg,
        _ => {
            let dest_reg = match used_registers.first() {
                Some(reg) => reg,
                None => {
                    used_registers = used_registers.with(Reg::Rax);
                    Reg::Rax
                }
            };
            instructions.push(format!("movq {},%{}", expr_loc, dest_reg));
            dest_reg
        }
    };
    instructions.push(format!("{} %{}", asm_op, dest_reg));
    Ok((Loc::Register(dest_reg), used_registers))
}

fn gen_unary_op(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    op: &UnaryOp,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    match op {
        UnaryOp::PreIncrement => gen_unary_op_pre_incdec(c, instructions, "incq", expr),
        UnaryOp::PreDecrement => gen_unary_op_pre_incdec(c, instructions, "decq", expr),
        UnaryOp::PostIncrement => gen_unary_op_post_incdec(c, instructions, "incq", expr),
        UnaryOp::PostDecrement => gen_unary_op_post_incdec(c, instructions, "decq", expr),
        UnaryOp::BitNot => gen_unary_op_non_assign(c, instructions, "notq", expr),
        UnaryOp::Negate => gen_unary_op_non_assign(c, instructions, "negq", expr),
    }
}

fn gen_bin_op(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    op: &BinOp,
    lhs: &Expr,
    rhs: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (lhs_loc, rhs_loc, used_registers) =
        gen_pre_op(c, instructions, lhs, rhs, registers_for_op(op))?;

    // Run the command!
    let (op_loc, op_registers) = gen_op_command(instructions, op, lhs_loc, rhs_loc);

    Ok((op_loc, used_registers.union(op_registers)))
}

fn gen_syscall(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    pos: &Pos,
    params: &Vec<Expr>,
) -> Result<(Loc, RegSet), CompErr> {
    if params.is_empty() || params.len() > 7 {
        return CompErr::err(pos, "syscall() must take between 1-7 arguments".to_string());
    }

    let mut used_registers = RegSet::of(Reg::Rax);

    for param in params.iter().rev() {
        let (param_loc, param_used_reg) = gen_expr(c, instructions, param)?;

        // TODO: Optimize better, quit monkeying around
        // No point pushing memory or immediate locations to the stack!
        instructions.push(format!("pushq {}", param_loc));

        used_registers = used_registers.union(param_used_reg);
    }

    for i in 0..params.len() {
        let reg = Reg::for_syscall_arg_num(i);
        instructions.push(format!("popq %{}", reg));
    }

    instructions.push("syscall".to_string());

    Ok((Loc::Register(Reg::Rax), used_registers))
}

// Returns the first 6 param locations
fn gen_call_params(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    params: &Vec<Expr>,
) -> Result<Vec<Loc>, CompErr> {
    // Evaluate backwards until the 7th var.
    // Since the 7th+ params have to be on the stack anyways
    for i in (6..params.len()).rev() {
        let param = &params[i];
        let (param_loc, _) = gen_expr(c, instructions, param)?;
        instructions.push(format!("pushq {}", param_loc));
    }

    let mut param_locs = vec![];

    for param in params.iter().take(std::cmp::min(6, params.len())) {
        let (param_loc, _) = gen_expr(c, instructions, param)?;

        if param_loc.is_reg() {
            instructions.push(format!("pushq {}", param_loc));
        }
        param_locs.push(param_loc);
    }

    Ok(param_locs)
}

/// Substitutes the given IDs in the AST to their respective values.
/// Used for macro expansion
fn substitute_id(body: &Expr, substitutions: &HashMap<String, Expr>) -> Result<Expr, CompErr> {
    Ok(match body {
        expr @ Expr::Str(_, _) => expr.clone(),
        expr @ Expr::Int(_, _) => expr.clone(),
        Expr::Id(pos, name) => match substitutions.get(name) {
            Some(value) => value.clone(),
            None => Expr::Id(pos.clone(), name.clone()),
        },
        Expr::Reference(pos, name) => match substitutions.get(name) {
            Some(_) => return CompErr::err(pos, "Cannot reference a macro arg".to_string()),
            None => Expr::Reference(pos.clone(), name.clone()),
        },
        Expr::Call(pos, callee, params) => {
            let sub_callee = substitute_id(callee, substitutions)?;
            let mut sub_params = vec![];
            for param in params {
                sub_params.push(substitute_id(param, substitutions)?);
            }
            Expr::Call(pos.clone(), Box::new(sub_callee), sub_params)
        }
        Expr::Assignment(pos, lhs, rhs) => {
            let sub_rhs = substitute_id(rhs, substitutions)?;
            Expr::Assignment(pos.clone(), lhs.clone(), Box::new(sub_rhs))
        }
        Expr::DerefAssignment(pos, lhs, rhs) => {
            let sub_lhs = substitute_id(lhs, substitutions)?;
            let sub_rhs = substitute_id(rhs, substitutions)?;
            Expr::DerefAssignment(pos.clone(), Box::new(sub_lhs), Box::new(sub_rhs))
        }
        Expr::UnaryOperator(pos, op, expr) => {
            let sub_expr = substitute_id(expr, substitutions)?;
            Expr::UnaryOperator(pos.clone(), op.clone(), Box::new(sub_expr))
        }
        Expr::BinOperator(pos, op, lhs, rhs) => {
            let sub_lhs = substitute_id(lhs, substitutions)?;
            let sub_rhs = substitute_id(rhs, substitutions)?;
            Expr::BinOperator(
                pos.clone(),
                op.clone(),
                Box::new(sub_lhs),
                Box::new(sub_rhs),
            )
        }
        Expr::Cond(pos, cond, truthy, falsey) => {
            let sub_cond = substitute_id(cond, substitutions)?;
            let sub_truthy = substitute_id(truthy, substitutions)?;
            let sub_falsey = substitute_id(falsey, substitutions)?;
            Expr::Cond(
                pos.clone(),
                Box::new(sub_cond),
                Box::new(sub_truthy),
                Box::new(sub_falsey),
            )
        }
        Expr::Dereference(pos, expr) => {
            let sub_expr = substitute_id(expr, substitutions)?;
            Expr::Dereference(pos.clone(), Box::new(sub_expr))
        }
    })
}

fn gen_call_macro(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    pos: &Pos,
    body: Expr,
    args: Vec<String>,
    params: &Vec<Expr>,
) -> Result<(Loc, RegSet), CompErr> {
    if args.len() != params.len() {
        return CompErr::err(
            pos,
            format!("This macro must accept {} arguments", args.len()),
        );
    }

    let mut substitutions = HashMap::new();
    for i in 0..args.len() {
        substitutions.insert(args[i].clone(), params[i].clone());
    }
    let sub_body = substitute_id(&body, &substitutions)?;
    gen_expr(c, instructions, &sub_body)
}

fn gen_call(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    pos: &Pos,
    callee_expr: &Expr,
    params: &Vec<Expr>,
) -> Result<(Loc, RegSet), CompErr> {
    match callee_expr {
        Expr::Id(_, name) if name == "syscall" => {
            return gen_syscall(c, instructions, pos, params);
        }
        _ => {}
    }

    let param_locs = gen_call_params(c, instructions, params)?;

    let callee = match callee_expr {
        Expr::Id(_, name) => match c.find_in_scope(name) {
            Some(ScopeEntry::Fun(arg_num)) => {
                if params.len() > *arg_num {
                    return CompErr::err(
                        pos,
                        format!("{} accepts at most {} arguments", name, arg_num),
                    );
                }
                name
            }
            Some(ScopeEntry::Var(loc)) => {
                instructions.push(format!("movq {},%rax", loc));
                "*%rax"
            }
            Some(ScopeEntry::Define(args, body)) => {
                let ac = args.clone();
                let bc = body.clone();
                let pc = pos.clone();
                return gen_call_macro(c, instructions, &pc, bc, ac, params);
            }
            None => return CompErr::err(pos, format!("{} not in scope", name)),
        },
        callee_expr => {
            let (callee_loc, _) = gen_expr(c, instructions, callee_expr)?;
            if callee_loc != Loc::Register(Reg::Rax) {
                instructions.push(format!("movq {},%rax", callee_loc));
            }
            "*%rax"
        }
    };

    for i in (0..std::cmp::min(6, params.len())).rev() {
        let reg = Reg::for_arg_num(i);
        let param_loc = &param_locs[i];

        if param_loc.is_reg() {
            instructions.push(format!("popq %{}", reg));
        } else {
            instructions.push(format!("movq {},%{}", param_loc, reg));
        }
    }

    instructions.push(format!("call {}", callee));

    if params.len() > 6 {
        let stack_arg_count = params.len() - 6;
        instructions.push(format!("addq ${}, %rsp", 8 * stack_arg_count));
    }

    // Assume we used all the registers, since we're calling an unknown function
    Ok((Loc::Register(Reg::Rax), RegSet::usable_caller_save()))
}

fn gen_reference(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    pos: &Pos,
    name: &String,
) -> Result<(Loc, RegSet), CompErr> {
    match c.find_in_scope(name) {
        Some(ScopeEntry::Var(Loc::Stack(offset))) => {
            let dest_reg = Reg::Rax;
            instructions.push(format!("leaq {}(%rbp),%{}", 8 * offset, dest_reg));
            Ok((Loc::Register(dest_reg), RegSet::of(dest_reg)))
        }
        Some(ScopeEntry::Var(other)) => {
            CompErr::err(pos, format!("Variable cannot be at {:?}!", other))
        }
        Some(ScopeEntry::Fun(_)) => {
            let dest_reg = Reg::Rax;
            instructions.push(format!("movq ${},%rax", name));
            Ok((Loc::Register(dest_reg), RegSet::of(dest_reg)))
        }
        Some(ScopeEntry::Define(_, _)) => {
            CompErr::err(pos, "#define value cannot be referenced".to_string())
        }
        None => CompErr::err(pos, format!("{} not in scope", name)),
    }
}

fn gen_dereference(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (target_loc, mut used_registers) = gen_expr(c, instructions, expr)?;

    let dest_reg = match target_loc {
        // Reuse the target register
        Loc::Register(dest_reg) => dest_reg,
        _ => {
            let new_reg = match used_registers.first() {
                Some(reg) => reg,
                None => {
                    used_registers = used_registers.with(Reg::Rax);
                    Reg::Rax
                }
            };
            instructions.push(format!("movq {},%{}", target_loc, new_reg));
            new_reg
        }
    };

    instructions.push(format!("movq (%{}),%{}", dest_reg, dest_reg));
    Ok((Loc::Register(dest_reg), used_registers))
}

// Generates the RHS instructions for an assignment
fn gen_expr_ass_rhs(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    rhs: &Expr,
) -> Result<(Reg, RegSet), CompErr> {
    let (rhs_loc, mut used_registers) = gen_expr(c, instructions, rhs)?;

    match rhs_loc {
        Loc::Register(reg) => Ok((reg, used_registers)),
        // Because we can't movq from memory to memory
        rhs_loc => {
            // Reuse a register if possible
            let rhs_reg = match used_registers.first() {
                Some(reg) => reg,
                None => {
                    used_registers = used_registers.with(Reg::Rax);
                    Reg::Rax
                }
            };

            instructions.push(format!("movq {},%{}", rhs_loc, rhs_reg));
            Ok((rhs_reg, used_registers))
        }
    }
}

fn gen_expr_ass(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    pos: &Pos,
    lhs_name: &String,
    rhs: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (rhs_reg, used_registers) = gen_expr_ass_rhs(c, instructions, rhs)?;

    match c.find_in_scope(lhs_name) {
        Some(ScopeEntry::Var(lhs_loc)) => {
            instructions.push(format!("movq %{},{}", rhs_reg, lhs_loc));
            Ok((lhs_loc.clone(), used_registers))
        }
        Some(ScopeEntry::Fun(_)) => CompErr::err(pos, "Cannot reassign a function".to_string()),
        Some(ScopeEntry::Define(_, _)) => {
            CompErr::err(pos, "Cannot reassign a #define value".to_string())
        }
        None => CompErr::err(pos, format!("Variable {} not in scope", lhs_name)),
    }
}

// FIXME: This register assignment technique is super similar in other places
// Abstract away!
fn gen_expr_deref_ass(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    lhs: &Expr,
    rhs: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (lhs_loc, mut used_registers) = gen_expr(c, instructions, lhs)?;
    let mut rhs_inst = vec![];
    let (rhs_reg, rhs_used) = gen_expr_ass_rhs(c, &mut rhs_inst, rhs)?;
    let safe_registers = get_safe_registers(rhs_used.clone());

    let lhs_dest_reg = match safe_registers.first() {
        Some(safe_reg) => match lhs_loc {
            Loc::Register(lhs_reg) if safe_registers.contains(lhs_reg) => Some(lhs_reg),
            lhs_loc => {
                instructions.push(format!("movq {},%{}", lhs_loc, safe_reg));
                used_registers = used_registers.with(safe_reg);
                Some(safe_reg)
            }
        },
        None => {
            // No safe registers! Push to stack!
            instructions.push(format!("pushq {}", lhs_loc));
            None
        }
    };

    instructions.append(&mut rhs_inst);
    used_registers = used_registers.union(rhs_used);

    // If needed, pull the LHS address back into a register
    let dest_reg = match lhs_dest_reg {
        Some(dest_reg) => dest_reg,
        // Pull the LHS address off the stack, into an unused register
        None => {
            let dest_reg = match rhs_reg {
                Reg::Rax => Reg::Rcx,
                _ => Reg::Rax,
            };
            instructions.push(format!("popq %{}", dest_reg));
            dest_reg
        }
    };

    // At this point
    // - instructions for both LHS and RHS are done
    // - The lhs address is at dest_reg
    // - The rhs value is at rhs_reg

    instructions.push(format!("movq %{},(%{})", rhs_reg, dest_reg));

    Ok((Loc::Register(rhs_reg), used_registers))
}

/// # Arguments
/// * `dest_reg` - For values > 2^32, this is the reg that will be used.
fn gen_int(instructions: &mut Vec<String>, signed: i64, dest_reg: Reg) -> (Loc, RegSet) {
    if signed < i32::MAX as i64 && signed >= i32::MIN as i64 {
        (Loc::Immediate(signed), RegSet::empty())
    } else {
        let unsigned = signed as u64;
        let upper = (unsigned & !((1 << 32) - 1)) >> 32;
        let lower = unsigned & ((1 << 32) - 1);

        // Since immediates can only be 32 bit ints at most
        instructions.push(format!("movq ${},%{}", upper, dest_reg));
        instructions.push(format!("shlq $32,%{}", dest_reg));
        instructions.push(format!("addq ${},%{}", lower, dest_reg));

        (Loc::Register(dest_reg), RegSet::of(dest_reg))
    }
}

fn gen_cond_expr(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    cond_expr: &Expr,
    true_expr: &Expr,
    false_expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    // Move the results to %rax, kinda arbitrarily
    let dest_reg = Reg::Rax;
    let dest_loc = Loc::Register(dest_reg);
    let mut used_registers = RegSet::of(dest_reg);

    let true_end_label = c.new_label("COND_TRUE_END");
    let false_end_label = c.new_label("COND_FALSE_END");
    gen_cond(c, instructions, cond_expr, &true_end_label)?;

    let (true_loc, true_used) = gen_expr(c, instructions, true_expr)?;
    used_registers = used_registers.union(true_used);
    if true_loc != dest_loc {
        instructions.push(format!("movq {},{}", true_loc, dest_loc));
    }
    instructions.push(format!("jmp {}", false_end_label));
    instructions.push(format!("{}:", true_end_label));

    let (false_loc, false_used) = gen_expr(c, instructions, false_expr)?;
    used_registers = used_registers.union(false_used);
    if false_loc != dest_loc {
        instructions.push(format!("movq {},{}", false_loc, dest_loc));
    }
    instructions.push(format!("{}:", false_end_label));

    Ok((dest_loc, used_registers))
}

/**
 * @return (instructions, location, used_registers)
 */
fn gen_expr(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    match expr {
        Expr::Int(_, value) => Ok(gen_int(instructions, *value, Reg::Rax)),
        Expr::Id(pos, name) => match c.find_in_scope(name) {
            Some(ScopeEntry::Var(loc)) => Ok((loc.clone(), RegSet::empty())),
            Some(ScopeEntry::Fun(_)) => CompErr::err(
                pos,
                format!(
                    "{} is a function, and can only be called or referenced",
                    name
                ),
            ),
            Some(ScopeEntry::Define(args, body)) => {
                if args.is_empty() {
                    let b = body.clone();
                    gen_expr(c, instructions, &b)
                } else {
                    CompErr::err(pos, format!("This macro must take {} args", args.len()))
                }
            }
            None => CompErr::err(pos, format!("Variable {} not in scope", name)),
        },
        Expr::Str(_, (file_id, string_index)) => {
            let label = label_for_string_id(*file_id, *string_index);
            instructions.push(format!("leaq {}(%rip),%rax", label));
            Ok((Loc::Register(Reg::Rax), RegSet::of(Reg::Rax)))
        }
        Expr::Assignment(pos, lhs, rhs) => gen_expr_ass(c, instructions, pos, lhs, rhs),
        Expr::DerefAssignment(_, lhs, rhs) => gen_expr_deref_ass(c, instructions, lhs, rhs),
        Expr::UnaryOperator(_, op, expr) => gen_unary_op(c, instructions, op, expr),
        Expr::BinOperator(_, op, lhs, rhs) => gen_bin_op(c, instructions, op, lhs, rhs),
        Expr::Call(pos, callee, params) => gen_call(c, instructions, pos, callee, params),
        Expr::Reference(pos, name) => gen_reference(c, instructions, pos, name),
        Expr::Dereference(_, expr) => gen_dereference(c, instructions, expr),
        Expr::Cond(_, cond, true_expr, false_expr) => {
            gen_cond_expr(c, instructions, cond, true_expr, false_expr)
        }
    }
}

fn gen_return_expr(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    expr: &Expr,
) -> Result<(), CompErr> {
    let (loc, _) = gen_expr(c, instructions, expr)?;

    // If the location is already rax, we don't need to move!
    if loc != Loc::Register(Reg::Rax) {
        instructions.push(format!("movq {},%rax", loc));
    }

    instructions.push("leave".to_string());
    instructions.push("ret".to_string());
    Ok(())
}

fn gen_return(instructions: &mut Vec<String>) {
    instructions.push("movq $0,%rax".to_string());
    instructions.push("leave".to_string());
    instructions.push("ret".to_string());
}

fn gen_cond_cmp(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    jump_command: &str,
    lhs: &Expr,
    rhs: &Expr,
    end_label: &String,
) -> Result<(), CompErr> {
    let (lhs_loc, rhs_loc, _) = gen_pre_op(c, instructions, lhs, rhs, RegSet::empty())?;
    instructions.push(format!("cmpq {},{}", rhs_loc, lhs_loc));
    instructions.push(format!("{} {}", jump_command, end_label));
    Ok(())
}

fn gen_cond(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    cond: &Expr,
    end_label: &String,
) -> Result<(), CompErr> {
    match cond {
        Expr::BinOperator(_, BinOp::Eq, lhs, rhs) => {
            gen_cond_cmp(c, instructions, "jne", lhs, rhs, end_label)
        }
        Expr::BinOperator(_, BinOp::Ne, lhs, rhs) => {
            gen_cond_cmp(c, instructions, "je", lhs, rhs, end_label)
        }
        Expr::BinOperator(_, BinOp::Gt, lhs, rhs) => {
            gen_cond_cmp(c, instructions, "jle", lhs, rhs, end_label)
        }
        Expr::BinOperator(_, BinOp::Ge, lhs, rhs) => {
            gen_cond_cmp(c, instructions, "jl", lhs, rhs, end_label)
        }
        Expr::BinOperator(_, BinOp::Lt, lhs, rhs) => {
            gen_cond_cmp(c, instructions, "jge", lhs, rhs, end_label)
        }
        Expr::BinOperator(_, BinOp::Le, lhs, rhs) => {
            gen_cond_cmp(c, instructions, "jg", lhs, rhs, end_label)
        }
        Expr::Int(_, value) => {
            if *value == 0 {
                instructions.push(format!("jmp {}", end_label));
                Ok(())
            } else {
                // For non-zero ints, no comparison needs to be made!
                Ok(())
            }
        }
        _ => {
            // Fallback to evaluating the entire conditional expression
            let (cond_loc, _) = gen_expr(c, instructions, cond)?;
            instructions.push(format!("cmpq $0,{}", cond_loc));
            instructions.push(format!("jz {}", end_label));
            Ok(())
        }
    }
}

fn gen_if(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    cond: &Expr,
    if_body: &Statement,
) -> Result<(), CompErr> {
    let if_end_label = c.new_label("IF_END");
    gen_cond(c, instructions, cond, &if_end_label)?;
    gen_statement(c, instructions, if_body)?;
    instructions.push(format!("{}:", if_end_label));
    Ok(())
}

fn gen_while(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    cond: &Expr,
    body: &Statement,
) -> Result<(), CompErr> {
    let while_begin_label = c.new_label("WHILE_BEGIN");
    instructions.push(format!("{}:", while_begin_label));
    let while_end_label = c.new_label("WHILE_END");

    c.break_dest_stack.push(while_end_label.clone());

    gen_cond(c, instructions, cond, &while_end_label)?;
    gen_statement(c, instructions, body)?;
    instructions.push(format!("jmp {}", while_begin_label));
    instructions.push(format!("{}:", while_end_label));

    c.break_dest_stack.pop();
    Ok(())
}

fn gen_if_else(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    cond: &Expr,
    if_body: &Statement,
    else_body: &Statement,
) -> Result<(), CompErr> {
    let if_end_label = c.new_label("IF_END");
    let else_end_label = c.new_label("ELSE_END");

    gen_cond(c, instructions, cond, &if_end_label)?;
    gen_statement(c, instructions, if_body)?;
    instructions.push(format!("jmp {}", else_end_label));
    instructions.push(format!("{}:", if_end_label));
    gen_statement(c, instructions, else_body)?;
    instructions.push(format!("{}:", else_end_label));
    Ok(())
}

fn gen_switch(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    cond: &Expr,
    body: &Vec<SwInner>,
) -> Result<(), CompErr> {
    let (expr_loc, _) = gen_expr(c, instructions, cond)?;
    // cmp requires the dest to be in a register
    let cond_loc = match expr_loc {
        loc @ Loc::Register(_) => loc,
        other => {
            instructions.push(format!("movq {},%rax", other));
            Loc::Register(Reg::Rax)
        }
    };

    // The register to store case values
    let case_reg = match cond_loc {
        Loc::Register(Reg::Rax) => Reg::Rcx,
        _ => Reg::Rax,
    };

    let mut used_case_values = HashSet::new();
    let mut default_label: Option<String> = None;
    let mut body_inst = vec![];

    let switch_end_label = c.new_label("SW_END");

    c.break_dest_stack.push(switch_end_label.clone());
    for inner in body {
        match inner {
            SwInner::Default(pos) => {
                if default_label.is_some() {
                    return CompErr::err(
                        pos,
                        "`default` label is already defined in switch".to_string(),
                    );
                }
                let label_name = c.new_label("SW_DEFAULT");
                body_inst.push(format!("{}:", label_name));
                default_label = Some(label_name);
            }
            SwInner::Case(pos, value) => {
                if used_case_values.contains(value) {
                    return CompErr::err(
                        pos,
                        format!("case {} is already defined in switch", value),
                    );
                }
                used_case_values.insert(value);

                let (case_loc, _) = gen_int(&mut body_inst, *value, case_reg);
                let label_name = c.new_label("SW_CASE");
                body_inst.push(format!("{}:", label_name));
                instructions.push(format!("cmpq {},{}", case_loc, cond_loc));
                instructions.push(format!("je {}", label_name));
            }
            SwInner::Statement(body) => gen_statement(c, &mut body_inst, body)?,
        }
    }
    c.break_dest_stack.pop();

    let inst = match default_label {
        Some(ref label_name) => format!("jmp {}", label_name),
        None => format!("jmp {}", switch_end_label),
    };

    // Default jump point
    instructions.push(inst);
    instructions.append(&mut body_inst);
    instructions.push(format!("{}:", switch_end_label));
    Ok(())
}

fn gen_auto(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    pos: &Pos,
    vars: &Vec<Var>,
) -> Result<(), CompErr> {
    for var in vars {
        // Guaranteed to exist because of the prepass
        let dest_loc = c.local_var_locs.get(var.name()).unwrap().clone();
        c.add_to_scope(pos, var.name().clone(), ScopeEntry::Var(dest_loc.clone()))?;

        match var {
            Var::Vec(_, size, initial) => {
                let offset = if let Loc::Stack(offset) = dest_loc {
                    offset
                } else {
                    panic!("Auto loc should always be on the stack!");
                };

                // The first value in the stack for a vector is a data pointer
                instructions.push(format!("leaq {}(%rbp),%rax", (offset - size) * 8));
                instructions.push(format!("movq %rax,{}", dest_loc));

                for (i, value) in initial.iter().enumerate() {
                    let val_dest_loc = Loc::Stack(offset - size + i as i64);

                    let (val_loc, _) = gen_int(instructions, *value, Reg::Rax);
                    instructions.push(format!("movq {},{}", val_loc, val_dest_loc));
                }
            }
            Var::Single(_, Some(value)) => {
                let (val_loc, _) = gen_int(instructions, *value, Reg::Rax);
                instructions.push(format!("movq {},{}", val_loc, dest_loc));
            }
            Var::Single(_, None) => {}
        }
    }
    Ok(())
}

fn gen_extern(c: &mut FunContext, pos: &Pos, vars: &Vec<String>) -> Result<(), CompErr> {
    for name in vars {
        if c.find_in_scope(name).is_some() {
            return CompErr::err(pos, format!("{} is already is scope", name));
        }

        match c.global_scope.get(name) {
            Some(ScopeEntry::Var(Loc::Data(_))) => {
                let entry = ScopeEntry::Var(Loc::Data(name.clone()));
                c.add_to_scope(pos, name.clone(), entry)?;
            }
            Some(ScopeEntry::Fun(_)) => {
                return CompErr::err(pos, format!("{} is a function, not a global var", name))
            }
            _ => return CompErr::err(pos, format!("Could not find definition for {}", name)),
        }
    }
    Ok(())
}

fn gen_statement(
    c: &mut FunContext,
    instructions: &mut Vec<String>,
    body: &Statement,
) -> Result<(), CompErr> {
    match body {
        Statement::Null => Ok(()),
        Statement::Break(pos) => match c.break_dest_stack.last() {
            Some(label) => {
                instructions.push("# break".to_string());
                instructions.push(format!("jmp {}", label));
                Ok(())
            }
            None => CompErr::err(pos, "Cannot break from this location".to_string()),
        },
        Statement::Goto(pos, name) => match c.labels.get(name) {
            Some(label) => {
                instructions.push(format!("# goto {}", label));
                instructions.push(format!("jmp {}", label));
                Ok(())
            }
            None => CompErr::err(
                pos,
                format!("Label '{}' not defined in this function", name),
            ),
        },
        // We preprocess the labels, so we know it must exist
        Statement::Label(_, name) => {
            instructions.push(format!("{}:", c.labels.get(name).unwrap()));
            Ok(())
        }
        Statement::Return => {
            instructions.push("# return".to_string());
            gen_return(instructions);
            Ok(())
        }
        Statement::ReturnExpr(expr) => {
            instructions.push("# return".to_string());
            gen_return_expr(c, instructions, expr)
        }
        Statement::Block(statements) => {
            c.new_scope();
            for statement in statements {
                gen_statement(c, instructions, statement)?;
            }
            c.drop_scope();
            Ok(())
        }
        Statement::Auto(pos, vars) => {
            instructions.push("# auto".to_string());
            gen_auto(c, instructions, pos, vars)
        }
        Statement::Extern(pos, vars) => {
            instructions.push("# extrn".to_string());
            gen_extern(c, pos, vars)
        }
        Statement::Expr(expr) => {
            instructions.push("# Expression statement".to_string());
            gen_expr(c, instructions, expr)?;
            Ok(())
        }
        Statement::If(cond, if_body, None) => {
            instructions.push("# if".to_string());
            gen_if(c, instructions, cond, if_body)
        }
        Statement::If(cond, if_body, Some(else_body)) => {
            instructions.push("# if".to_string());
            gen_if_else(c, instructions, cond, if_body, else_body)
        }
        Statement::While(cond, body) => {
            instructions.push("# while".to_string());
            gen_while(c, instructions, cond, body)
        }
        Statement::Switch(cond, body) => {
            instructions.push("# switch".to_string());
            gen_switch(c, instructions, cond, body)
        }
    }
}

fn gen_fun(c: &mut FunContext, function: &RSFunction) -> Result<Vec<String>, CompErr> {
    let pos = &function.pos;
    let args = &function.args;
    let body = &function.body;

    c.new_scope();
    let mut instructions = vec![];
    // Save base pointer, since it's callee-saved
    instructions.push("pushq %rbp".to_string());
    instructions.push("movq %rsp,%rbp".to_string());

    // Prepare initial stack memory
    alloc_args(c, &mut instructions, pos, args)?;
    prepass_gen(
        c,
        &mut instructions,
        body,
        1 + std::cmp::min(6, args.len() as i64),
    )?;

    gen_statement(c, &mut instructions, body)?;

    let trailing_ret = match instructions.last() {
        Some(instruction) => instruction == "ret",
        _ => false,
    };

    if !trailing_ret {
        gen_return(&mut instructions);
    }

    c.drop_scope();
    Ok(instructions)
}

// Prepass to find the global scope, before we start evaluating things
fn root_prepass<'a>(
    functions: &Vec<RSFunction>,
    variables: &'a Vec<RSVariable>,
    defines: Vec<RSDefine>,
) -> Result<(HashMap<String, ScopeEntry>, Vec<&'a Var>), CompErr> {
    let mut scope = HashMap::new();
    let mut root_vars = Vec::<&Var>::new();

    for variable in variables {
        let var = &variable.var;

        let name = var.name();
        if scope.contains_key(name) {
            return CompErr::err(&variable.pos, format!("{} already in root scope", name));
        }
        root_vars.push(var);

        scope.insert(name.clone(), ScopeEntry::Var(Loc::Data(name.clone())));
    }

    for function in functions {
        let name = &function.name;
        if scope.contains_key(name) {
            return CompErr::err(&function.pos, format!("{} already in root scope", name));
        }
        scope.insert(name.clone(), ScopeEntry::Fun(function.args.len()));
    }

    for define in defines {
        let name = &define.name;
        if scope.contains_key(name) {
            return CompErr::err(&define.pos, format!("{} already in root scope", name));
        }

        scope.insert(name.clone(), ScopeEntry::Define(define.args, define.body));
    }

    Ok((scope, root_vars))
}

fn generate_data_segment(root_vars: &Vec<&Var>, w: &mut dyn Write) -> Result<(), std::io::Error> {
    writeln!(w, ".data")?;
    for var in root_vars {
        write!(w, "{}:\n    ", var.name())?;

        match var {
            Var::Single(_, None) => {
                writeln!(w, ".skip 8")?;
            }
            Var::Single(_, Some(value)) => {
                writeln!(w, ".quad {}", value)?;
            }
            Var::Vec(_, size, initial) => {
                if initial.is_empty() {
                    // +1 for the vec pointer
                    writeln!(w, ".skip {}", (1 + size) * 8)?;
                } else {
                    // One extra at the begining for vec pointer
                    write!(w, ".quad 0")?;

                    for value in initial.iter() {
                        write!(w, ",{}", value)?;
                    }

                    // Fill the rest with zeros
                    for _ in initial.len()..*size as usize {
                        write!(w, ",0")?;
                    }
                    writeln!(w)?;
                }
            }
        };
    }
    Ok(())
}

fn generate_start(root_vars: &Vec<&Var>, w: &mut dyn Write) -> Result<(), std::io::Error> {
    writeln!(w, ".text\n.global _start\n_start:")?;

    for var in root_vars {
        match var {
            Var::Single(_, _) => {}
            Var::Vec(name, _, _) => {
                // Initialize vec pointers
                // For consistency with stack vectors, data vectors are pointers
                writeln!(w, "    leaq {}(%rip),%rax", name)?;
                writeln!(w, "    addq $8,%rax")?;
                writeln!(w, "    movq %rax,{}(%rip)", name)?;
            }
        }
    }

    writeln!(w, "    movq (%rsp),%rdi")?; // Pass argc as first `main` arg
    writeln!(w, "    leaq 8(%rsp),%rsi")?; // Pass argv as second `main` arg
    writeln!(w, "    call main")?;
    writeln!(w, "    movq %rax,%rdi")?;
    writeln!(w, "    movq $60,%rax")?;
    writeln!(w, "    syscall")?;

    Ok(())
}

fn generate_strings(
    strings: &Vec<(usize, Vec<Vec<char>>)>,
    w: &mut dyn Write,
) -> Result<(), std::io::Error> {
    writeln!(w, ".text")?;
    // Prevent constant strings from being modified
    writeln!(w, ".section .rodata")?;
    for (file_id, file_strings) in strings {
        for (string_index, string_chars) in file_strings.iter().enumerate() {
            let label = label_for_string_id(*file_id, string_index);
            // TODO: Print nicely instead of packing into quads
            let string_quads = pack_chars(string_chars);
            write!(w, "{}:\n    .quad {}", label, string_quads[0])?;
            for quad in string_quads.iter().skip(1) {
                write!(w, ",{}", quad)?;
            }
            writeln!(w)?;
        }
    }
    Ok(())
}

fn gen(
    strings: Vec<(usize, Vec<Vec<char>>)>,
    functions: Vec<RSFunction>,
    variables: Vec<RSVariable>,
    defines: Vec<RSDefine>,
    writer: &mut dyn Write,
) -> Result<(), CompErr> {
    let mut w = BufWriter::new(writer);

    let (global_scope, root_vars) = root_prepass(&functions, &variables, defines)?;

    CompErr::from_io_res(generate_data_segment(&root_vars, &mut w))?;
    CompErr::from_io_res(generate_strings(&strings, &mut w))?;
    CompErr::from_io_res(generate_start(&root_vars, &mut w))?;

    let pool = Arc::new((
        Mutex::new(CodeGenPool {
            running_fibers: 0,
            functions,
            results: vec![],
            errors: vec![],
        }),
        Condvar::new(),
    ));

    let thread_count = logical_cpu_count();
    let arc_global_scope = Arc::new(global_scope);

    let mut handles = Vec::with_capacity(thread_count);
    for _ in 0..thread_count {
        let th_pool_mutex = pool.clone();
        let th_global_scope = arc_global_scope.clone();

        handles.push(thread::spawn(move || {
            codegen_fiber(th_global_scope, th_pool_mutex);
        }))
    }

    while let Some((func_name, instructions)) = pop_pool_result(&pool) {
        CompErr::from_io_res(writeln!(w, "{}:", func_name))?;
        for instruction in instructions {
            CompErr::from_io_res(writeln!(w, "    {}", instruction))?;
        }
    }

    for handle in handles {
        handle.join().unwrap();
    }

    let guard = pool.0.lock().unwrap();

    if !guard.errors.is_empty() {
        return Err(guard.errors[0].clone());
    }

    // Might be redundant. TODO: Double check!!!
    let guard_iter = guard.results.iter();
    for (func_name, instructions) in guard_iter {
        CompErr::from_io_res(writeln!(w, "{}:", func_name))?;
        for instruction in instructions {
            CompErr::from_io_res(writeln!(w, "    {}", instruction))?;
        }
    }
    Ok(())
}

fn pop_pool_result(pool: &Arc<(Mutex<CodeGenPool>, Condvar)>) -> Option<(String, Vec<String>)> {
    let (mutex, cvar) = pool.as_ref();
    let mut guard = mutex.lock().unwrap();

    while guard.results.is_empty() && guard.running_fibers != 0 {
        guard = cvar.wait(guard).unwrap();
    }
    guard.results.pop()
}

fn unpool_function(pool: &Arc<(Mutex<CodeGenPool>, Condvar)>) -> Option<(usize, RSFunction)> {
    let mut guard = pool.0.lock().unwrap();
    let func_id = guard.functions.len();

    if !guard.errors.is_empty() {
        return None;
    }

    guard.functions.pop().map(|fun| {
        guard.running_fibers += 1;
        (func_id, fun)
    })
}

fn codegen_fiber(
    global_scope: Arc<HashMap<String, ScopeEntry>>,
    pool: Arc<(Mutex<CodeGenPool>, Condvar)>,
) {
    loop {
        match unpool_function(&pool) {
            Some((func_id, fun)) => {
                let mut c = FunContext {
                    global_scope: &global_scope,
                    fun_scope: HashMap::new(),
                    block_vars: vec![],
                    local_var_locs: HashMap::new(),
                    labels: HashMap::new(),
                    func_id,
                    label_counter: 0,
                    break_dest_stack: vec![],
                };

                match gen_fun(&mut c, &fun) {
                    Ok(instructions) => {
                        let (mutex, cvar) = pool.as_ref();
                        let mut guard = mutex.lock().unwrap();
                        guard.results.push((fun.name, instructions));
                        guard.running_fibers -= 1;
                        cvar.notify_all();
                    }
                    Err(err) => {
                        let (mutex, cvar) = pool.as_ref();
                        let mut guard = mutex.lock().unwrap();
                        guard.errors.push(err);
                        guard.running_fibers -= 1;
                        cvar.notify_all();
                    }
                }
            }
            None => {
                pool.1.notify_all();
                return;
            }
        }
    }
}

pub fn generate(parse_result: ParseResult, writer: &mut dyn Write) {
    match gen(
        parse_result.strings,
        parse_result.functions,
        parse_result.variables,
        parse_result.defines,
        writer,
    ) {
        Ok(_) => {}
        Err(err) => {
            print_comp_error(&parse_result.file_paths, &err);
            std::process::exit(1);
        }
    }
}
