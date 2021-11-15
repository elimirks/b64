use std::collections::HashMap;
use std::collections::LinkedList;
use std::collections::HashSet;
use std::io::BufWriter;
use std::io::Write;

use crate::ast::*;
use crate::memory::*;

struct FunContext {
    variables: HashMap<String, Loc>,
    // So we never run out of unique labels
    label_counter: usize,
}

impl FunContext {
    fn new_label(&mut self, prefix: &str) -> String {
        self.label_counter += 1;
        // By prefixing with '.', it guarantees no collisions with user labels
        format!(".{}_{}", prefix, self.label_counter)
    }
}

/**
 * Allocates stack memory for auto variables
 * @param body The function body to search for auto declarations
 * @param offset The positive offset from rbp (how much space came before this)
 */
fn alloc_autos(c: &mut FunContext, body: &Statement, offset: i64) -> LinkedList<String> {
    let mut instructions = LinkedList::new();
    let mut stack = vec!(body);
    let mut autos_size = 0;

    // DFS to find them all
    while !stack.is_empty() {
        match stack.pop().unwrap() {
            Statement::Auto(vars) => {
                for var in vars {
                    c.variables.insert(
                        var.clone(),
                        Loc::Stack(-offset - autos_size)
                    );
                    autos_size += 1;
                }
            },
            Statement::Block(statements) => {
                for s in statements {
                    stack.push(s);
                }
            },
            _ => {},
        }
    }

    if autos_size > 0 {
        instructions.push_back(format!("subq ${}, %rsp", 8 * autos_size));
    }

    instructions
}

// Allocates the necessary args on the stack
fn alloc_args(c: &mut FunContext, args: &Vec<String>) -> LinkedList<String> {
    let mut instructions = LinkedList::new();
    for i in 0..args.len() {
        if i < 6 {
            let register = register_for_arg_num(i);
            instructions.push_back(format!("pushq %{}", register));
            c.variables.insert(
                args[i].clone(),
                Loc::Stack(-(i as i64) - 1)
            );
        } else {
            c.variables.insert(
                args[i].clone(),
                Loc::Stack((i as i64) - 4)
            );
        }
    }
    instructions
}

fn register_for_arg_num(num: usize) -> Reg {
    match num {
        0 => Reg::Rdi,
        1 => Reg::Rsi,
        2 => Reg::Rdx,
        3 => Reg::Rcx,
        4 => Reg::R8,
        5 => Reg::R9,
        _ => {
            panic!("arg num {} is not stored in a register", num)
        }
    }
}

// Assumes dest is a register
fn gen_op_command(op: &Op, src: Loc, dest: Loc) -> (LinkedList<String>, Loc, Option<Reg>) {
    let mut instructions = LinkedList::new();

    match op {
        Op::Add => {
            instructions.push_back(format!("addq {},{}", src, dest));
            (instructions, dest, None)
        },
        Op::Sub => {
            instructions.push_back(format!("subq {},{}", src, dest));
            (instructions, dest, None)
        },
        Op::Equals => {
            instructions.push_back(format!("cmp {},{}", src, dest));
            instructions.push_back(format!("lahf"));
            // Select zero flag
            instructions.push_back(format!("andq $0x4000,%rax"));
            (instructions, Loc::Register(Reg::Rax), Some(Reg::Rax))
        },
    }
}

/**
 * Plans evaluation strategy for an LHS and RHS expression.
 * It will try to store the LHS value in a register if possible.
 * Otherwise it stores to the stack.
 * @return (instructions, lhs_loc, rhs_loc, used_registers)
 */
fn gen_pre_op(c: &FunContext, lhs: &Expr, rhs: &Expr) -> (LinkedList<String>, Loc, Loc, Vec<Reg>) {
    // Generate instructions for RHS first so we know which registers are safe
    let (mut rhs_ins, rhs_loc, mut used_registers) = gen_expr(c, rhs);
    let mut avail_registers: HashSet<&Reg> =
        USABLE_CALLER_SAVE_REG.into_iter().collect();
    for reg in &used_registers {
        avail_registers.remove(&reg.clone());
    }

    // TODO: If lhs_loc is not even in a register, don't bother moving it
    let (mut lhs_ins, lhs_loc, mut lhs_registers) = gen_expr(c, lhs);
    used_registers.append(&mut lhs_registers);

    let new_lhs_loc = match avail_registers.iter().next() {
        // If there are safe registers available, store the lhs there
        Some(dest_reg) => {
            // TODO: If the lhs_loc is already in a safe register, don't move it
            used_registers.push(**dest_reg);
            lhs_ins.push_back(format!("movq {},%{}", lhs_loc, dest_reg));
            lhs_ins.append(&mut rhs_ins);
            Loc::Register(**dest_reg)
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

    (lhs_ins, new_lhs_loc, rhs_loc, used_registers)
}

fn gen_op(c: &FunContext, op: &Op, lhs: &Expr, rhs: &Expr) -> (LinkedList<String>, Loc, Vec<Reg>) {
    let (mut instructions, dest_loc, rhs_loc, mut used_registers) =
        gen_pre_op(c, lhs, rhs);

    // Run the command!
    let (mut op_ins, op_loc, op_reg) =
        gen_op_command(op, rhs_loc, dest_loc);

    if let Some(reg) = op_reg {
        used_registers.push(reg);
    }

    instructions.append(&mut op_ins);

    (instructions, op_loc, used_registers)
}

fn gen_call(c: &FunContext, name: &String, params: &Vec<Expr>) -> (LinkedList<String>, Loc, Vec<Reg>) {
    let mut instructions = LinkedList::new();

    // Evaluate backwards until the 7th var.
    // Since the 7th+ params have to be on the stack anyways
    for i in (6..params.len()).rev() {
        let param = &params[i];
        let (mut param_instructions, param_loc, _) = gen_expr(c, param);
        instructions.append(&mut param_instructions);
        instructions.push_back(format!("pushq {}", param_loc));
    }

    let mut param_locs = vec!();

    for i in (0..std::cmp::min(6, params.len())).rev() {
        let param = &params[i];
        let (mut param_instructions, param_loc, _) = gen_expr(c, param);
        instructions.append(&mut param_instructions);

        param_locs.push(param_loc);

        if param_loc.is_reg() {
            instructions.push_back(format!("pushq {}", param_loc));
        }
    }

    for i in 0..std::cmp::min(6, params.len()) {
        let reg = register_for_arg_num(i);
        let param_loc = param_locs[i];

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
    let used_vars: Vec<Reg> = USABLE_CALLER_SAVE_REG.to_vec();
    (instructions, Loc::Register(Reg::Rax), used_vars)
}

/**
 * @return (instructions, location, used_registers)
 */
fn gen_expr<'a>(c: &'a FunContext, expr: &'a Expr) -> (LinkedList<String>, Loc, Vec<Reg>) {
    match expr {
        Expr::Int(value) => {
            (LinkedList::new(), Loc::Immediate(*value), vec!())
        },
        Expr::Id(name) => {
            match c.variables.get(name) {
                Some(location) => (LinkedList::new(), *location, vec!()),
                None => panic!("Variable {} not in scope", name),
            }
        },
        Expr::Assignment(lhs_name, rhs) => {
            let (mut instructions, rhs_loc, used_registers) = gen_expr(c, rhs);

            match c.variables.get(lhs_name) {
                Some(lhs_loc) => {
                    instructions.push_back(format!("movq {},{}", rhs_loc, lhs_loc));
                    (instructions, *lhs_loc, used_registers)
                },
                None => panic!("Variable {} not in scope", lhs_name),
            }
        },
        Expr::Operator(op, lhs, rhs) => gen_op(c, op, lhs, rhs),
        Expr::Call(name, params)     => gen_call(c, name, params),
    }
}

fn gen_return_expr(c: &FunContext, expr: &Expr) -> LinkedList<String> {
    let (mut instructions, loc, _) = gen_expr(c, &expr);

    // If the location is already rax, we don't need to move!
    if loc != Loc::Register(Reg::Rax) {
        instructions.push_back(format!("movq {},%rax", loc));
    }

    instructions.push_back("leave".to_string());
    instructions.push_back("ret".to_string());
    instructions
}

fn gen_return() -> LinkedList<String> {
    let mut instructions = LinkedList::new();
    instructions.push_back("movq $0,%rax".to_string());
    instructions.push_back("leave".to_string());
    instructions.push_back("ret".to_string());
    instructions
}

fn gen_cond_comparison(
    c: &mut FunContext,
    cond: &Expr,
    end_label: &String
) -> LinkedList<String> {
    match cond {
        Expr::Operator(Op::Equals, lhs, rhs) => {
            let (mut instructions, lhs_loc, rhs_loc, _) =
                gen_pre_op(c, lhs, rhs);
            instructions.push_back(format!("cmp {},{}", lhs_loc, rhs_loc));
            instructions.push_back(format!("je {}", end_label));
            instructions
        },
        _ => {
            let (mut instructions, cond_loc, _) = gen_expr(c, cond);
            instructions.push_back(format!("cmp $0,{}", cond_loc));
            instructions.push_back(format!("je {}", end_label));
            instructions
        },
    }
}

fn gen_if(
    c: &mut FunContext,
    cond: &Expr,
    if_body: &Statement
) -> LinkedList<String> {
    let if_end_label = c.new_label("IF");
    let mut instructions = gen_cond_comparison(c, cond, &if_end_label);
    instructions.append(&mut gen_statement(c, if_body));
    instructions.push_back(format!("{}:", if_end_label));
    instructions
}

fn gen_if_else(
    c: &mut FunContext,
    cond: &Expr,
    if_body: &Statement,
    else_body: &Statement,
) -> LinkedList<String> {
    let if_end_label = c.new_label("IF");
    let else_end_label = c.new_label("ELSE");
    let mut instructions = gen_cond_comparison(c, cond, &if_end_label);
    instructions.append(&mut gen_statement(c, if_body));
    instructions.push_back(format!("jmp {}", else_end_label));
    instructions.push_back(format!("{}:", if_end_label));
    instructions.append(&mut gen_statement(c, else_body));
    instructions.push_back(format!("{}:", else_end_label));
    instructions
}

// Returns true if the last statement is a return
fn gen_statement(c: &mut FunContext, body: &Statement) -> LinkedList<String> {
    match body {
        Statement::Null => LinkedList::new(),
        Statement::Return => gen_return(),
        Statement::ReturnExpr(expr) => gen_return_expr(c, expr),
        Statement::Block(statements) => {
            let mut instructions = LinkedList::new();
            for statement in statements {
                instructions.append(&mut gen_statement(c, statement))
            }
            instructions
        },
        Statement::Auto(_) => LinkedList::new(),
        Statement::Expr(expr) => {
            let (instructions, _, _) = gen_expr(c, expr);
            instructions
        },
        Statement::If(cond, if_body, None) => gen_if(c, cond, if_body),
        Statement::If(cond, if_body, Some(else_body)) => gen_if_else(c, cond, if_body, else_body),
    }
}

fn gen_fun(args: Vec<String>, body: Statement) -> LinkedList<String> {
    let mut c = FunContext {
        variables: HashMap::new(),
        label_counter: 0,
    };

    let mut instructions = LinkedList::new();
    // Save base pointer, since it's callee-saved
    instructions.push_back(format!("pushq %rbp"));
    instructions.push_back(format!("movq %rsp,%rbp"));

    // Prepare initial stack memory
    instructions.append(&mut alloc_args(&mut c, &args));
    instructions.append(
        &mut alloc_autos(&mut c, &body, 1 + std::cmp::min(6, args.len() as i64))
    );

    instructions.append(&mut gen_statement(&mut c, &body));

    let trailing_ret = match instructions.back() {
        Some(instruction) => instruction == "ret",
        _ => false,
    };

    if !trailing_ret {
        instructions.append(&mut gen_return());
    }

    instructions
}

pub fn generate(statements: Vec<RootStatement>, writer: &mut dyn Write) {
    let mut w = BufWriter::new(writer);

    // Call main, use the return value as the exit status
    writeln!(w, ".text\n\
                 .global _start\n\
                 _start:\n\
                 call main\n\
                 movq %rax,%rbx\n\
                 movq $1,%rax\n\
                 int $0x80"
    ).expect("Failed writing to output");

    for statement in statements {
        match statement {
            RootStatement::Function(name, args, body) => {
                let instructions = gen_fun(args, body);
                writeln!(w, "{}:", name)
                    .expect("Failed writing to output");
                for instruction in instructions {
                    writeln!(w, "    {}", instruction)
                        .expect("Failed writing to output");
                }
            },
        }
    }
}
