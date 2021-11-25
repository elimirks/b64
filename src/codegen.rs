use std::collections::HashMap;
use std::collections::LinkedList;
use std::io::BufWriter;
use std::io::Write;

use crate::parser::*;
use crate::ast::*;
use crate::memory::*;

#[derive(Debug)]
enum ScopeEntry {
    // Contains the number of args
    Fun(usize),
    Var(Loc),
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
    label_counter: usize,
    // Stack of end labels where to tell break where to jump to
    break_dest_stack: Vec<String>,
}

impl FunContext<'_> {
    fn new_label(&mut self, prefix: &str) -> String {
        self.label_counter += 1;
        // By prefixing with '.', it guarantees no collisions with user labels
        format!(".{}_{}", prefix, self.label_counter)
    }

    fn new_scope(&mut self) {
        self.block_vars.push(vec!());
    }

    fn drop_scope(&mut self) {
        // We know it must exist, since a scope is pushed on function entry
        for var in self.block_vars.pop().unwrap() {
            self.fun_scope.remove(&var);
        }
    }

    fn add_to_scope(
        &mut self, pos: &Pos, name: String, entry: ScopeEntry
    ) -> Result<(), CompErr> {
        if self.fun_scope.contains_key(&name) {
            return CompErr::err(pos, format!(
                "{} is already in defined in this scope", name));
        }
        self.fun_scope.insert(name.clone(), entry);
        self.block_vars.last_mut().unwrap().push(name);
        Ok(())
    }

    fn find_in_scope(&self, name: &String) -> Option<&ScopeEntry> {
        if let Some(entry) = self.fun_scope.get(name) {
            return Some(entry);
        }
        match self.global_scope.get(name) {
            // Only allow referencing global vars when users specify "extrn"
            Some(ScopeEntry::Var(Loc::Data(_))) => None,
            other                               => other,
        }
    }
}

// Make it easier to create linked lists
macro_rules! ll {
    ($($entry:expr),*) => {{
        #[allow(unused_mut)]
        let mut list = LinkedList::new();
        $(
            list.push_back($entry);
        )*
        list
    }};
}

/**
 * Allocates stack memory for auto local_var_locs and finds labels.
 * @param body The function body to search for auto declarations
 * @param offset The positive offset from rbp (how much space came before this)
 */
fn prepass_gen(
    c: &mut FunContext, body: &Statement, offset: i64
) -> Result<LinkedList<String>, CompErr> {
    let mut instructions = ll!();
    let mut stack = vec!(body);
    let mut autos_size = 0;

    // DFS to find them all
    while !stack.is_empty() {
        match stack.pop().unwrap() {
            Statement::Label(pos, name) => {
                if c.labels.contains_key(name) {
                    return CompErr::err(pos, format!(
                        "Label {} already defined in this function", name));
                }

                let l = c.new_label(&format!("LAB_{}", name).to_string());
                c.labels.insert(name.clone(), l);
            },
            Statement::Auto(pos, vars) => {
                for var in vars {
                    let name = var.name();

                    if c.local_var_locs.contains_key(name) {
                        return CompErr::err(pos, format!(
                            "{} already defined in this function", name));
                    }

                    let size = match var {
                        Var::Vec(_, vec_size, _) => 1 + vec_size,
                        Var::Single(_, _)        => 1,
                    };

                    c.local_var_locs.insert(
                        name.clone(),
                        Loc::Stack(-offset - autos_size)
                    );
                    autos_size += size;
                }
            },
            Statement::Block(statements) => {
                for s in statements {
                    stack.push(s);
                }
            },
            Statement::If(_, if_body, Some(else_body)) => {
                stack.push(if_body);
                stack.push(else_body);
            },
            Statement::If(_, body, None) => stack.push(body),
            Statement::While(_, body)    => stack.push(body),
            _ => {},
        }
    }

    if autos_size > 0 {
        instructions.push_back(format!("subq ${}, %rsp", 8 * autos_size));
    }

    Ok(instructions)
}

// Allocates the necessary args on the stack
fn alloc_args(
    c: &mut FunContext, pos: &Pos, args: &Vec<String>
) -> Result<LinkedList<String>, CompErr> {
    let mut instructions = ll!();
    for i in 0..args.len() {
        let loc = if i < 6 {
            let register = Reg::for_arg_num(i);
            instructions.push_back(format!("pushq %{}", register));
            Loc::Stack(-(i as i64) - 1)
        } else {
            Loc::Stack((i as i64) - 4)
        };

        c.local_var_locs.insert(args[i].clone(), loc.clone());
        c.add_to_scope(pos, args[i].clone(), ScopeEntry::Var(loc))?;
    }
    Ok(instructions)
}

fn gen_op_cmp(
    command: &str, lhs_loc: Loc, rhs_loc: Loc
) -> (LinkedList<String>, Loc, RegSet) {
    let mut instructions = ll!(format!("cmpq {},{}", rhs_loc, lhs_loc),
                               format!("movq $0,{}", lhs_loc));

    if let Loc::Register(lhs_reg) = lhs_loc {
        instructions.push_back(format!("{} %{}", command, lhs_reg.low_byte()));
    } else {
        panic!("LHS must be a register");
    }

    (instructions, lhs_loc, RegSet::empty())
}

fn gen_op_single(
    command: &str, lhs_loc: Loc, rhs_loc: Loc
) -> (LinkedList<String>, Loc, RegSet) {
    (ll!(format!("{} {},{}", command, rhs_loc, lhs_loc)),
     lhs_loc,
     RegSet::empty())
}

/**
 * Generate instructions to diving (or mod) two numbers
 * As per x86 idivq, the results always go to rax and rdx
 */
fn gen_op_pre_div(
    lhs_loc: Loc, init_rhs_loc: Loc
) -> (LinkedList<String>, RegSet) {
    let mut instructions = ll!();
    // rax and rdx are always used for div or mod
    let mut used_registers = RegSet::of(Reg::Rax).with(Reg::Rdx);

    let should_move_rhs = match init_rhs_loc {
        Loc::Register(Reg::Rax) => true,
        Loc::Register(Reg::Rdx) => true,
        Loc::Immediate(_)       => true,
        _                       => false,
    };

    // Move rhs to a new register if necessary
    let rhs_loc = if should_move_rhs {
        // Make sure we don't override LHS
        let dest_reg = match lhs_loc {
            Loc::Register(Reg::Rcx) => Reg::Rdi,
            _                       => Reg::Rcx,
        };

        used_registers = used_registers.with(dest_reg);
        instructions.push_back(format!(
            "movq {},%{}", init_rhs_loc, dest_reg
        ));

        Loc::Register(dest_reg)
    } else {
        init_rhs_loc
    };

    match lhs_loc {
        Loc::Register(Reg::Rax) => {},
        lhs_loc => instructions.push_back(format!("movq {},%rax", lhs_loc)),
    }
    instructions.push_back("movq $0,%rdx".to_string());
    instructions.push_back(format!("idivq {}", rhs_loc));

    (instructions, used_registers)
}

fn gen_op_mod(
    lhs_loc: Loc, rhs_loc: Loc
) -> (LinkedList<String>, Loc, RegSet) {
    let (instructions, used_registers) = gen_op_pre_div(lhs_loc, rhs_loc);
    (instructions, Loc::Register(Reg::Rdx), used_registers)
}

fn gen_op_div(
    lhs_loc: Loc, rhs_loc: Loc
) -> (LinkedList<String>, Loc, RegSet) {
    let (instructions, used_registers) = gen_op_pre_div(lhs_loc, rhs_loc);
    (instructions, Loc::Register(Reg::Rax), used_registers)
}

/**
 * Generates instructions for applying the operator to the given lhs & rhs.
 * May override either lhs_loc or rhs_loc if they are registers.
 * ASSUMES lhs_loc is already in a register
 * @return (instructions, dest_loc, extra_registers)
 */
fn gen_op_command(
    op: &BinOp, lhs_loc: Loc, rhs_loc: Loc
) -> (LinkedList<String>, Loc, RegSet) {
    match op {
        BinOp::Add        => gen_op_single("addq", lhs_loc, rhs_loc),
        BinOp::Sub        => gen_op_single("subq", lhs_loc, rhs_loc),
        BinOp::Mod        => gen_op_mod(lhs_loc, rhs_loc),
        BinOp::Div        => gen_op_div(lhs_loc, rhs_loc),
        BinOp::ShiftRight => gen_op_single("shr", lhs_loc, rhs_loc),
        BinOp::ShiftLeft  => gen_op_single("shl", lhs_loc, rhs_loc),
        BinOp::Eq         => gen_op_cmp("sete", lhs_loc, rhs_loc),
        BinOp::Ne         => gen_op_cmp("setne", lhs_loc, rhs_loc),
        BinOp::Le         => gen_op_cmp("setle", lhs_loc, rhs_loc),
        BinOp::Lt         => gen_op_cmp("setl", lhs_loc, rhs_loc),
        BinOp::Ge         => gen_op_cmp("setge", lhs_loc, rhs_loc),
        BinOp::Gt         => gen_op_cmp("setg", lhs_loc, rhs_loc),
        BinOp::Assign     => panic!("Assignments should be parsed differently"),
    }
}

fn get_safe_registers(used_registers: RegSet) -> RegSet {
    RegSet::usable_caller_save().subtract(&used_registers)
}

/**
 * Plans evaluation strategy for an LHS and RHS expression.
 * It will try to store the LHS value in a register if possible.
 * Otherwise it stores to the stack.
 * The returned lhs_loc is guaranteed to be in a register
 * @return (instructions, lhs_loc, rhs_loc, used_registers)
 */
fn gen_pre_op(
    c: &FunContext, lhs: &Expr, rhs: &Expr
) -> Result<(LinkedList<String>, Loc, Loc, RegSet), CompErr> {
    // Generate instructions for RHS first so we know which registers are safe
    let (mut rhs_ins, rhs_loc, mut used_registers) = gen_expr(c, rhs)?;
    let safe_registers = get_safe_registers(used_registers.clone());

    let (mut lhs_ins, lhs_loc, lhs_registers) = gen_expr(c, lhs)?;
    used_registers = used_registers.union(lhs_registers);

    // If the lhs_loc is already in a safe register, don't move it
    if let Loc::Register(lhs_reg) = lhs_loc {
        if safe_registers.contains(lhs_reg) {
            lhs_ins.append(&mut rhs_ins);
            return Ok((lhs_ins, lhs_loc, rhs_loc, used_registers));
        }
    }

    let new_lhs_loc = match safe_registers.first() {
        // If there are safe registers available, store the lhs there
        Some(dest_reg) => {
            used_registers = used_registers.with(dest_reg);
            lhs_ins.push_back(format!("movq {},%{}", lhs_loc, dest_reg));
            lhs_ins.append(&mut rhs_ins);
            Loc::Register(dest_reg)
        },
        // Nowhere is safe! Store LHS on the stack
        None => {
            let lhs_in_reg = match lhs_loc {
                Loc::Register(_) => true,
                _                => false,
            };

            if lhs_in_reg {
                lhs_ins.push_back(format!("pushq {}", lhs_loc));
            }

            lhs_ins.append(&mut rhs_ins);

            // Don't need to update used_registers because...
            // we already know everything is used!
            let new_lhs_loc = match rhs_loc {
                Loc::Register(Reg::Rax) => Loc::Register(Reg::Rcx),
                _                       => Loc::Register(Reg::Rax),
            };

            if lhs_in_reg {
                lhs_ins.push_back(format!("popq {}", new_lhs_loc));
            } else {
                lhs_ins.push_back(format!("movq {},{}", lhs_loc, new_lhs_loc));
            }
            new_lhs_loc
        },
    };

    Ok((lhs_ins, new_lhs_loc, rhs_loc, used_registers))
}

fn gen_unary_op_assign(
    c: &FunContext, asm_op: &str, expr: &Expr
) -> Result<(LinkedList<String>, Loc, RegSet), CompErr> {
    let (mut instructions, expr_loc, used_registers) = gen_expr(c, expr)?;

    match expr_loc {
        Loc::Register(_) | Loc::Immediate(_) =>
            return CompErr::err(&expr.pos(), format!(
                "`++` or `--` must operate on a variable")),
        _ => {},
    };

    instructions.push_back(format!("{} {}", asm_op, expr_loc));
    Ok((instructions, expr_loc, used_registers))
}

fn gen_unary_op_non_assign(
    c: &FunContext, asm_op: &str, expr: &Expr
) -> Result<(LinkedList<String>, Loc, RegSet), CompErr> {
    let (mut instructions, expr_loc, mut used_registers) = gen_expr(c, expr)?;
    let dest_reg = match expr_loc {
        Loc::Register(reg) => reg,
        _ => {
            let dest_reg = match used_registers.first() {
                Some(reg) => reg,
                None      => {
                    used_registers = used_registers.with(Reg::Rax);
                    Reg::Rax
                },
            };
            instructions.push_back(format!("movq {},%{}", expr_loc, dest_reg));
            dest_reg
        },
    };
    instructions.push_back(format!("{} %{}", asm_op, dest_reg));
    Ok((instructions, Loc::Register(dest_reg), used_registers))
}

fn gen_unary_op(
    c: &FunContext, op: &UnaryOp, expr: &Expr
) -> Result<(LinkedList<String>, Loc, RegSet), CompErr> {
    match op {
        UnaryOp::Increment => gen_unary_op_assign(c, "incq", expr),
        UnaryOp::Decrement => gen_unary_op_assign(c, "decq", expr),
        UnaryOp::BitNot    => gen_unary_op_non_assign(c, "notq", expr),
        UnaryOp::Negate    => gen_unary_op_non_assign(c, "negq", expr),
    }
}

fn gen_bin_op(
    c: &FunContext, op: &BinOp, lhs: &Expr, rhs: &Expr
) -> Result<(LinkedList<String>, Loc, RegSet), CompErr> {
    let (mut instructions, lhs_loc, rhs_loc, mut used_registers) =
        gen_pre_op(c, lhs, rhs)?;

    // Run the command!
    let (mut op_ins, op_loc, op_registers) =
        gen_op_command(op, lhs_loc, rhs_loc);

    instructions.append(&mut op_ins);
    used_registers = used_registers.union(op_registers);

    Ok((instructions, op_loc, used_registers))
}

fn gen_syscall(
    c: &FunContext, pos: &Pos, params: &Vec<Expr>
) -> Result<(LinkedList<String>, Loc, RegSet), CompErr> {
    if params.len() == 0 || params.len() > 7 {
        return CompErr::err(pos, format!(
            "syscall() must take between 1-7 arguments"));
    }

    let mut instructions = ll!();
    let mut used_registers = RegSet::empty();

    for param in params.iter().rev() {
        let (mut param_inst, param_loc, param_used_reg) =
            gen_expr(c, &param)?;

        instructions.append(&mut param_inst);

        // TODO: Optimize better, quit monkeying around
        // No point pushing memory or immediate locations to the stack!
        instructions.push_back(format!("pushq {}", param_loc));

        used_registers = used_registers.union(param_used_reg);
    }

    for i in 0..params.len() {
        let reg = Reg::for_syscall_arg_num(i);
        instructions.push_back(format!("popq %{}", reg));
    }

    instructions.push_back(format!("syscall"));

    Ok((instructions, Loc::Register(Reg::Rax), used_registers))
}

fn gen_call(
    c: &FunContext, pos: &Pos, name: &String, params: &Vec<Expr>
) -> Result<(LinkedList<String>, Loc, RegSet), CompErr> {
    if name == "syscall" {
        return gen_syscall(c, pos, params);
    }

    match c.find_in_scope(name) {
        Some(ScopeEntry::Fun(arg_num)) => {
            if params.len() != *arg_num {
                return CompErr::err(pos, format!(
                    "{} must accept {} arguments", name, arg_num));
            }
        },
        Some(ScopeEntry::Var(_)) => {
            return CompErr::err(pos, format!(
                "{} is not a function", name));
        },
        None => return CompErr::err(pos, format!(
            "{} not in scope", name)),
    }

    let mut instructions = ll!();

    // Evaluate backwards until the 7th var.
    // Since the 7th+ params have to be on the stack anyways
    for i in (6..params.len()).rev() {
        let param = &params[i];
        let (mut param_instructions, param_loc, _) = gen_expr(c, param)?;
        instructions.append(&mut param_instructions);
        instructions.push_back(format!("pushq {}", param_loc));
    }

    let mut param_locs = vec!();

    for i in 0..std::cmp::min(6, params.len()) {
        let param = &params[i];
        let (mut param_instructions, param_loc, _) = gen_expr(c, param)?;
        instructions.append(&mut param_instructions);

        if param_loc.is_reg() {
            instructions.push_back(format!("pushq {}", param_loc));
        }
        param_locs.push(param_loc);
    }

    for i in (0..std::cmp::min(6, params.len())).rev() {
        let reg = Reg::for_arg_num(i);
        let param_loc = &param_locs[i];

        if param_loc.is_reg() {
            instructions.push_back(format!("popq %{}", reg));
        } else {
            instructions.push_back(format!("movq {},%{}", param_loc, reg));
        }
    }

    instructions.push_back(format!("call {}", name));

    if params.len() > 6 {
        let stack_arg_count = params.len() - 6;
        instructions.push_back(format!("addq ${}, %rsp", 8 * stack_arg_count));
    }

    // Assume we used all the registers, since we're calling an unknown function
    Ok((instructions, Loc::Register(Reg::Rax), RegSet::usable_caller_save()))
}

fn gen_reference(
    c: &FunContext, pos: &Pos, name: &String
) -> Result<(LinkedList<String>, Loc, RegSet), CompErr> {
    match c.find_in_scope(name) {
        Some(ScopeEntry::Var(Loc::Stack(offset))) => {
            let dest_reg = Reg::Rax;
            let mut instructions = ll!(format!("movq %rbp,%{}", dest_reg));

            if *offset < 0 {
                instructions.push_back(format!(
                    "subq ${},%{}", 8 * -offset, dest_reg));
            } else if *offset > 0 {
                instructions.push_back(format!(
                    "addq ${},%{}", 8 * offset, dest_reg));
            }

            Ok((instructions, Loc::Register(dest_reg), RegSet::of(dest_reg)))
        },
        Some(ScopeEntry::Var(other)) => CompErr::err(pos, format!(
            "Variable cannot be at {:?}!", other)),
        Some(ScopeEntry::Fun(_)) => CompErr::err(pos, format!(
            "Cannot reference a function (yet) {}", name)),
        None => CompErr::err(pos, format!(
            "{} not in scope", name)),
    }
}

fn gen_dereference(
    c: &FunContext, expr: &Expr
) -> Result<(LinkedList<String>, Loc, RegSet), CompErr> {
    let (mut instructions, target_loc, mut used_registers) = gen_expr(c, expr)?;

    let dest_reg = match used_registers.first() {
        Some(reg) => reg,
        None => {
            used_registers = used_registers.with(Reg::Rax);
            Reg::Rax
        },
    };

    instructions.push_back(format!("movq {},%{}", target_loc, dest_reg));
    instructions.push_back(format!("movq (%{}),%{}", dest_reg, dest_reg));

    Ok((instructions, Loc::Register(dest_reg), used_registers))
}

// Generates the RHS instructions for an assignment
fn gen_expr_ass_rhs(
    c: &FunContext, rhs: &Expr
) -> Result<(LinkedList<String>, Reg, RegSet), CompErr> {
    let (mut instructions, rhs_loc, mut used_registers) = gen_expr(c, rhs)?;

    match rhs_loc {
        Loc::Register(reg) => Ok((instructions, reg, used_registers)),
        // Because we can't movq from memory to memory
        rhs_loc => {
            // Reuse a register if possible
            let rhs_reg = match used_registers.first() {
                Some(reg) => reg,
                None => {
                    used_registers = used_registers.with(Reg::Rax);
                    Reg::Rax
                },
            };

            instructions.push_back(format!("movq {},%{}", rhs_loc, rhs_reg));
            Ok((instructions, rhs_reg, used_registers))
        },
    }
}

fn gen_expr_ass(
    c: &FunContext, pos: &Pos, lhs_name: &String, rhs: &Expr
) -> Result<(LinkedList<String>, Loc, RegSet), CompErr> {
    let (mut instructions, rhs_reg, used_registers) =
        gen_expr_ass_rhs(c, rhs)?;

    match c.find_in_scope(lhs_name) {
        Some(ScopeEntry::Var(lhs_loc)) => {
            instructions.push_back(format!("movq %{},{}", rhs_reg, lhs_loc));
            Ok((instructions, lhs_loc.clone(), used_registers))
        },
        Some(ScopeEntry::Fun(_)) => 
            CompErr::err(pos, format!("Cannot reassign a function")),
        None =>
            CompErr::err(pos, format!("Variable {} not in scope", lhs_name)),
    }
}

// FIXME: This register assignment technique is super similar in other places
// Abstract away!
fn gen_expr_deref_ass(
    c: &FunContext, lhs: &Expr, rhs: &Expr
) -> Result<(LinkedList<String>, Loc, RegSet), CompErr> {
    let (mut instructions, lhs_loc, mut used_registers) = gen_expr(c, lhs)?;
    let (mut rhs_inst, rhs_reg, rhs_used) = gen_expr_ass_rhs(c, rhs)?;
    let safe_registers = get_safe_registers(rhs_used.clone());

    let lhs_dest_reg = match safe_registers.first() {
        Some(safe_reg) => match lhs_loc {
            Loc::Register(lhs_reg) if safe_registers.contains(lhs_reg) =>
                Some(lhs_reg),
            lhs_loc => {
                instructions.push_back(format!(
                    "movq {},%{}", lhs_loc, safe_reg
                ));
                used_registers = used_registers.with(safe_reg);
                Some(safe_reg)
            },
        },
        None => {
            // No safe registers! Push to stack!
            instructions.push_back(format!("pushq {}", lhs_loc));
            None
        },
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
                _        => Reg::Rax,
            };
            instructions.push_back(format!("popq %{}", dest_reg));
            dest_reg
        },
    };

    // At this point
    // - instructions for both LHS and RHS are done
    // - The lhs address is at dest_reg
    // - The rhs value is at rhs_reg

    instructions.push_back(format!("movq %{},(%{})", rhs_reg, dest_reg));

    Ok((instructions, Loc::Register(rhs_reg), used_registers))
}

fn gen_int(signed: i64) -> (LinkedList<String>, Loc, RegSet) {
    if signed < i32::MAX as i64 && signed >= i32::MIN as i64 {
        (ll!(), Loc::Immediate(signed), RegSet::empty())
    } else {
        let unsigned = signed as u64;
        let dest_reg = Reg::Rax;

        let upper = (unsigned & !((1 << 32) - 1)) >> 32;
        let lower = unsigned & ((1 << 32) - 1);

        // Since immediates can only be 32 bit ints at most
        let instructions = ll!(
            format!("movq ${},%{}", upper, dest_reg),
            format!("shlq $32,%{}", dest_reg),
            format!("addq ${},%{}", lower, dest_reg)
        );

        (instructions, Loc::Register(dest_reg), RegSet::of(dest_reg))
    }
}

/**
 * @return (instructions, location, used_registers)
 */
fn gen_expr(
    c: &FunContext, expr: &Expr
) -> Result<(LinkedList<String>, Loc, RegSet), CompErr> {
    match expr {
        Expr::Int(_, value) => Ok(gen_int(*value)),
        Expr::Id(pos, name) => {
            match c.find_in_scope(name) {
                Some(ScopeEntry::Var(loc)) =>
                    Ok((ll!(), loc.clone(), RegSet::empty())),
                Some(ScopeEntry::Fun(_)) =>
                    CompErr::err(pos, format!(
                        "{} is a function, and can only be called", name)),
                None => CompErr::err(pos, format!(
                    "Variable {} not in scope", name)),
            }
        },
        Expr::Assignment(pos, lhs, rhs)    => gen_expr_ass(c, pos, lhs, rhs),
        Expr::DerefAssignment(_, lhs, rhs) => gen_expr_deref_ass(c, lhs, rhs),
        Expr::UnaryOperator(_, op, expr)   => gen_unary_op(c, op, expr),
        Expr::BinOperator(_, op, lhs, rhs) => gen_bin_op(c, op, lhs, rhs),
        Expr::Call(pos, name, params)      => gen_call(c, pos, name, params),
        Expr::Reference(pos, name)         => gen_reference(c, pos, name),
        Expr::Dereference(_, expr)         => gen_dereference(c, expr),
    }
}

fn gen_return_expr(
    c: &FunContext, expr: &Expr
) -> Result<LinkedList<String>, CompErr> {
    let (mut instructions, loc, _) = gen_expr(c, &expr)?;

    // If the location is already rax, we don't need to move!
    if loc != Loc::Register(Reg::Rax) {
        instructions.push_back(format!("movq {},%rax", loc));
    }

    instructions.push_back("leave".to_string());
    instructions.push_back("ret".to_string());
    Ok(instructions)
}

fn gen_return() -> LinkedList<String> {
    ll!("movq $0,%rax".to_string(),
        "leave".to_string(),
        "ret".to_string())
}

fn gen_cond_cmp(
    c: &mut FunContext,
    jump_command: &str,
    lhs: &Expr,
    rhs: &Expr,
    end_label: &String
) -> Result<LinkedList<String>, CompErr> {
    let (mut instructions, lhs_loc, rhs_loc, _) = gen_pre_op(c, lhs, rhs)?;
    instructions.push_back(format!("cmpq {},{}", rhs_loc, lhs_loc));
    instructions.push_back(format!("{} {}", jump_command, end_label));
    Ok(instructions)
}

fn gen_cond(
    c: &mut FunContext,
    cond: &Expr,
    end_label: &String
) -> Result<LinkedList<String>, CompErr> {
    match cond {
        Expr::BinOperator(_, BinOp::Eq, lhs, rhs) =>
            gen_cond_cmp(c, "jne", lhs, rhs, end_label),
        Expr::BinOperator(_, BinOp::Ne, lhs, rhs) =>
            gen_cond_cmp(c, "je", lhs, rhs, end_label),
        Expr::BinOperator(_, BinOp::Gt, lhs, rhs) =>
            gen_cond_cmp(c, "jle", lhs, rhs, end_label),
        Expr::BinOperator(_, BinOp::Ge, lhs, rhs) =>
            gen_cond_cmp(c, "jl", lhs, rhs, end_label),
        Expr::BinOperator(_, BinOp::Lt, lhs, rhs) =>
            gen_cond_cmp(c, "jge", lhs, rhs, end_label),
        Expr::BinOperator(_, BinOp::Le, lhs, rhs) =>
            gen_cond_cmp(c, "jg", lhs, rhs, end_label),
        _ => {
            // Fallback to evaluating the entire conditional expression
            let (mut instructions, cond_loc, _) = gen_expr(c, cond)?;
            instructions.push_back(format!("cmpq $0,{}", cond_loc));
            instructions.push_back(format!("jz {}", end_label));
            Ok(instructions)
        },
    }
}

fn gen_if(
    c: &mut FunContext,
    cond: &Expr,
    if_body: &Statement
) -> Result<LinkedList<String>, CompErr> {
    let if_end_label = c.new_label("IF_END");
    let mut instructions = gen_cond(c, cond, &if_end_label)?;
    instructions.append(&mut gen_statement(c, if_body)?);
    instructions.push_back(format!("{}:", if_end_label));
    Ok(instructions)
}

fn gen_while(
    c: &mut FunContext,
    cond: &Expr,
    body: &Statement
) -> Result<LinkedList<String>, CompErr> {
    let mut instructions = ll!();
    let while_begin_label = c.new_label("WHILE_BEGIN");
    instructions.push_back(format!("{}:", while_begin_label));
    let while_end_label = c.new_label("WHILE_END");

    c.break_dest_stack.push(while_end_label.clone());

    instructions.append(&mut gen_cond(c, cond, &while_end_label)?);
    instructions.append(&mut gen_statement(c, body)?);
    instructions.push_back(format!("jmp {}", while_begin_label));
    instructions.push_back(format!("{}:", while_end_label));

    c.break_dest_stack.pop();

    Ok(instructions)
}

fn gen_if_else(
    c: &mut FunContext,
    cond: &Expr,
    if_body: &Statement,
    else_body: &Statement,
) -> Result<LinkedList<String>, CompErr> {
    let if_end_label = c.new_label("IF_END");
    let else_end_label = c.new_label("ELSE_END");
    let mut instructions = gen_cond(c, cond, &if_end_label)?;
    instructions.append(&mut gen_statement(c, if_body)?);
    instructions.push_back(format!("jmp {}", else_end_label));
    instructions.push_back(format!("{}:", if_end_label));
    instructions.append(&mut gen_statement(c, else_body)?);
    instructions.push_back(format!("{}:", else_end_label));
    Ok(instructions)
}

fn gen_auto(
    c: &mut FunContext, pos: &Pos, vars: &Vec<Var>
) -> Result<LinkedList<String>, CompErr> {
    let mut instructions = ll!();
    for var in vars {
        // Guaranteed to exist because of the prepass
        let dest_loc = c.local_var_locs.get(var.name()).unwrap().clone();
        c.add_to_scope(pos, var.name().clone(), ScopeEntry::Var(dest_loc.clone()))?;

        match var {
            Var::Vec(_, size, initial) => {
                // The first value in the stack for a vector is a data pointer
                instructions.push_back(format!("leaq {}(%rbp),%rax", size * 8));
                instructions.push_back(format!("movq %rax,{}", dest_loc));

                for i in 0..initial.len() {
                    let value = initial[i];
                    let val_dest_loc = Loc::Stack(size + i as i64);

                    let (mut val_inst, val_loc, _) = gen_int(value);
                    instructions.append(&mut val_inst);
                    instructions.push_back(format!(
                        "movq {},{}", val_loc, val_dest_loc));
                }
            },
            Var::Single(_, Some(value)) => {
                let (mut val_inst, val_loc, _) = gen_int(*value);
                instructions.append(&mut val_inst);
                instructions.push_back(format!(
                    "movq {},{}", val_loc, dest_loc));
            },
            Var::Single(_, None) => {},
        }
    }
    Ok(instructions)
}

fn gen_extern(
    c: &mut FunContext, pos: &Pos, vars: &Vec<String>
) -> Result<LinkedList<String>, CompErr> {
    for name in vars {
        if !c.find_in_scope(name).is_none() {
            return CompErr::err(pos, format!(
                "{} is already is scope", name));
        }

        match c.global_scope.get(name) {
            Some(ScopeEntry::Var(Loc::Data(_))) => {
                let entry = ScopeEntry::Var(Loc::Data(name.clone()));
                c.add_to_scope(pos, name.clone(), entry)?;
            },
            Some(ScopeEntry::Fun(_)) => return CompErr::err(pos, format!(
                "{} is a function, not a global var", name)),
            _ => return CompErr::err(pos, format!(
                "Could not find definition for {}", name)),
        }
    }

    Ok(ll!())
}

fn gen_statement(
    c: &mut FunContext, body: &Statement
) -> Result<LinkedList<String>, CompErr> {
    match body {
        Statement::Null => Ok(ll!()),
        Statement::Break(pos) => {
            match c.break_dest_stack.last() {
                Some(label) => Ok(ll!(format!("jmp {}", label))),
                None => CompErr::err(pos, format!(
                    "Cannot break from this location")),
            }
        },
        Statement::Goto(pos, name) => {
            match c.labels.get(name) {
                Some(label) => Ok(ll!(format!("jmp {}", label))),
                None =>
                    CompErr::err(pos, format!(
                        "Label '{}' not defined in this function", name)),
            }
        },
        // We preprocess the labels, so we know it must exist
        Statement::Label(_, name) =>
            Ok(ll!(format!("{}:", c.labels.get(name).unwrap()))),
        Statement::Return => Ok(gen_return()),
        Statement::ReturnExpr(expr) => gen_return_expr(c, expr),
        Statement::Block(statements) => {
            c.new_scope();
            let mut instructions = ll!();
            for statement in statements {
                instructions.append(&mut gen_statement(c, statement)?)
            }
            c.drop_scope();
            Ok(instructions)
        },
        Statement::Auto(pos, vars)         => gen_auto(c, pos, vars),
        Statement::Extern(pos, vars)       => gen_extern(c, pos, vars),
        Statement::Expr(expr)              => Ok(gen_expr(c, expr)?.0),
        Statement::If(cond, if_body, None) => gen_if(c, cond, if_body),
        Statement::While(cond, body)       => gen_while(c, cond, body),
        Statement::If(cond, if_body, Some(else_body)) =>
            gen_if_else(c, cond, if_body, else_body),
    }
}

fn gen_fun(
    c: &mut FunContext, pos: &Pos, args: &Vec<String>, body: &Statement
) -> Result<LinkedList<String>, CompErr> {
    c.new_scope();
    // Save base pointer, since it's callee-saved
    let mut instructions = ll!(format!("pushq %rbp"),
                               format!("movq %rsp,%rbp"));

    // Prepare initial stack memory
    instructions.append(&mut alloc_args(c, pos, &args)?);
    instructions.append(
        &mut prepass_gen(c, &body, 1 + std::cmp::min(6, args.len() as i64))?
    );

    instructions.append(&mut gen_statement(c, &body)?);

    let trailing_ret = match instructions.back() {
        Some(instruction) => instruction == "ret",
        _ => false,
    };

    if !trailing_ret {
        instructions.append(&mut gen_return());
    }

    c.drop_scope();
    Ok(instructions)
}

// Prepass to find the global scope, before we start evaluating things
fn root_prepass(
    statements: &Vec<RootStatement>
) -> Result<(HashMap<String, ScopeEntry>, Vec<&Var>), CompErr> {
    let mut scope = HashMap::new();
    let mut root_vars = Vec::<&Var>::new();

    for statement in statements {
        match statement {
            RootStatement::Function(pos, name, args, _) => {
                if scope.contains_key(name) {
                    return CompErr::err(
                        pos, format!("{} already in root scope", name));
                }

                scope.insert(name.clone(), ScopeEntry::Fun(args.len()));
            },
            RootStatement::Variable(pos, var) => {
                let name = var.name();
                if scope.contains_key(name) {
                    return CompErr::err(
                        pos, format!("{} already in root scope", name));
                }
                root_vars.push(var);

                scope.insert(name.clone(),
                             ScopeEntry::Var(Loc::Data(name.clone())));
            },
        }
    }
    Ok((scope, root_vars))
}

fn generate_data_segment(
    root_vars: &Vec<&Var>, w: &mut dyn Write
) -> Result<(), std::io::Error> {
    writeln!(w, ".data")?;
    for var in root_vars {
        write!(w, "{}:\n    ", var.name())?;

        match var {
            Var::Single(_, None) => {
                writeln!(w, ".skip 8")?;
            },
            Var::Single(_, Some(value)) => {
                writeln!(w, ".quad {}", value)?;
            },
            Var::Vec(_, size, initial) => {
                if initial.is_empty() {
                    // +1 for the vec pointer
                    writeln!(w, ".skip {}", (1 + size) * 8)?;
                } else {
                    // One extra at the beggining for vec pointer
                    write!(w, ".quad 0")?;

                    for i in 0..initial.len() {
                        let value = initial[i];
                        write!(w, ",{}", value)?;
                    }

                    // Fill the rest with zeros
                    for _ in initial.len()..*size as usize {
                        write!(w, ",0")?;
                    }
                    write!(w, "\n")?;
                }
            },
        };
    }
    Ok(())
}

fn generate_start(
    root_vars: &Vec<&Var>, w: &mut dyn Write
) -> Result<(), std::io::Error> {
    writeln!(w, "{}\n{}\n{}",
             ".text",
             ".global _start",
             "_start:")?;

    for var in root_vars {
        match var {
            Var::Single(_, _) => {},
            Var::Vec(name, _, _) => {
                // Initialize vec pointers
                // For consistency with stack vectors, data vectors are pointers
                writeln!(w, "    leaq {}(%rip),%rax", name)?;
                writeln!(w, "    addq $8,%rax")?;
                writeln!(w, "    movq %rax,{}(%rip)", name)?;
            }
        }
    }

    writeln!(w, "    call main")?;
    writeln!(w, "    movq %rax,%rdi")?;
    writeln!(w, "    movq $60,%rax")?;
    writeln!(w, "    syscall")?;

    Ok(())
}

fn gen(
    statements: &Vec<RootStatement>, writer: &mut dyn Write
) -> Result<(), CompErr> {
    let mut w = BufWriter::new(writer);

    let (global_scope, root_vars) = root_prepass(&statements)?;

    CompErr::from_io_res(generate_data_segment(&root_vars, &mut w))?;
    CompErr::from_io_res(generate_start(&root_vars, &mut w))?;

    let mut label_counter = 0;

    for statement in statements {
        match statement {
            RootStatement::Function(pos, name, args, body) => {
                let mut c = FunContext {
                    global_scope: &global_scope,
                    fun_scope: HashMap::new(),
                    block_vars: vec!(),
                    local_var_locs: HashMap::new(),
                    labels: HashMap::new(),
                    label_counter: label_counter,
                    break_dest_stack: vec!(),
                };

                let instructions = gen_fun(&mut c, &pos, args, body)?;

                CompErr::from_io_res(writeln!(w, "{}:", name))?;
                for instruction in instructions {
                    CompErr::from_io_res(writeln!(w, "    {}", instruction))?;
                }

                label_counter = c.label_counter;
            },
            // Vars are generated in the prepass
            RootStatement::Variable(_, _) => {},
        }
    }

    Ok(())
}

pub fn generate(parse_result: &ParseResult, writer: &mut dyn Write) {
    match gen(&parse_result.root_statements, writer) {
        Ok(_) => {},
        Err(err) => {
            print_comp_error(parse_result, &err);
            std::process::exit(1);
        },
    }
}
