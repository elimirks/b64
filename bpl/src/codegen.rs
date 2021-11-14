use std::collections::HashMap;
use std::collections::LinkedList;

use crate::ast::*;
use crate::memory::*;

struct FunContext {
    variables: HashMap<String, Loc>,
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

fn op_to_command(op: &Op) -> String {
    match op {
        Op::Add => "addq".to_string(),
        Op::Sub => "subq".to_string(),
    }
}

fn gen_op(c: &FunContext, op: &Op, lhs: &Expr, rhs: &Expr) -> (LinkedList<String>, Loc) {
    /*
     * TODO: At some point this should get optimized...
     * In many cases it needlessly relies on the stack.
     * A better strategy would be to track which registers "rhs" needs.
     * And only move the lhs result if it overlaps with the "rhs" registers.
     * If all registers are used by "rhs", then in _that_ case push to the stack.
     */
    let (mut instructions, lhs_loc) = gen_expr(c, lhs);
    instructions.push_back(format!("pushq {}", lhs_loc));

    let (mut rhs_instructions, rhs_loc) = gen_expr(c, rhs);
    instructions.append(&mut rhs_instructions);

    let command = op_to_command(op);

    let dest_loc = match rhs_loc {
        Loc::Register(Reg::Rax) => Loc::Register(Reg::Rcx),
        _                       => Loc::Register(Reg::Rax),
    };

    instructions.push_back(format!("popq {}", dest_loc));
    instructions.push_back(format!("{} {}, {}", command, rhs_loc, dest_loc));

    (instructions, dest_loc)
}

// Returns the location of the final expression
fn gen_expr<'a>(c: &'a FunContext, expr: &'a Expr) -> (LinkedList<String>, Loc) {
    match expr {
        Expr::Int(value) => {
            (LinkedList::new(), Loc::Immediate(*value))
        },
        Expr::Id(name) => {
            match c.variables.get(name) {
                Some(location) => (LinkedList::new(), *location),
                None => panic!("Variable {} not in scope", name),
            }
        },
        Expr::Assignment(lhs_name, rhs) => {
            let (mut instructions, rhs_loc) = gen_expr(c, rhs);

            match c.variables.get(lhs_name) {
                Some(lhs_loc) => {
                    instructions.push_back(format!("movq {}, {}", rhs_loc, lhs_loc));
                    (instructions, *lhs_loc)
                },
                None => panic!("Variable {} not in scope", lhs_name),
            }
        },
        Expr::Operator(op, lhs, rhs) => gen_op(c, op, lhs, rhs),
    }
}

fn gen_return(c: &FunContext, expr: &Expr) -> LinkedList<String> {
    let (mut instructions, loc) = gen_expr(c, &expr);

    // If the location is already rax, we don't need to move!
    if loc != Loc::Register(Reg::Rax) {
        instructions.push_back(format!("movq {}, %rax", loc));
    }

    instructions.push_back("leave".to_string());
    instructions.push_back("retq".to_string());
    instructions
}

// Returns true if the last statement is a return
fn gen_fun_body(c: &FunContext, body: &Statement) -> LinkedList<String> {
    match body {
        Statement::Return(expr) => gen_return(c, expr),
        Statement::Block(statements) => {
            let mut instructions = LinkedList::new();
            for statement in statements {
                instructions.append(&mut gen_fun_body(c, statement))
            }
            instructions
        },
        Statement::Auto(_) => LinkedList::new(),
        Statement::Expr(expr) => {
            let (instructions, _) = gen_expr(c, expr);
            instructions
        },
    }
}

fn gen_fun(args: Vec<String>, body: Statement) -> LinkedList<String> {
    let mut c = FunContext {
        variables: HashMap::new(),
    };

    let mut instructions = LinkedList::new();
    // Save base pointer, since it's callee-saved
    instructions.push_back(format!("pushq %rbp"));
    instructions.push_back(format!("movq %rsp, %rbp"));

    // Prepare initial stack memory
    instructions.append(&mut alloc_args(&mut c, &args));
    instructions.append(
        &mut alloc_autos(&mut c, &body, 1 + std::cmp::min(6, args.len() as i64))
    );

    instructions.append(&mut gen_fun_body(&c, &body));

    instructions.push_back("leave".to_string());
    instructions.push_back("retq".to_string());

    //println!("Entry variable allocation: {:?}:", c.variables);
    instructions
}

pub fn generate(statements: Vec<RootStatement>) {
    for statement in statements {
        match statement {
            RootStatement::Function(name, args, body) => {
                let instructions = gen_fun(args, body);
                println!("{}:", name);
                for instruction in instructions {
                    println!("  {}", instruction);
                }
            },
            //other => println!("Can't compile this yet: {:?}", other),
        }
    }
}
