use std::collections::HashMap;

use crate::ast::*;
use crate::memory::*;

struct FunContext {
    variables: HashMap<String, VarLoc>,
    // Size of the stack, excluding things above (and including) return address
}

/**
 * Allocates stack memory for auto variables
 * @param body The function body to search for auto declarations
 * @param offset The positive offset from rbp (how much space came before this)
 */
fn alloc_autos(c: &mut FunContext, body: &Statement, offset: i64) {
    let mut stack = vec!(body);
    let mut autos_size = 0;

    // DFS to find them all
    while !stack.is_empty() {
        match stack.pop().unwrap() {
            Statement::Auto(vars) => {
                for var in vars {
                    c.variables.insert(
                        var.clone(),
                        VarLoc::Stack(-offset - autos_size)
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
        println!("  subq ${}, %rsp", 8 * autos_size);
    }
}

// Allocates the necessary args on the stack
fn alloc_args(c: &mut FunContext, args: &Vec<String>) {
    for i in 0..args.len() {
        if i < 6 {
            let register = register_for_arg_num(i);
            println!("  pushq %{}", register);
            c.variables.insert(
                args[i].clone(),
                VarLoc::Stack(-(i as i64) - 1)
            );
        } else {
            c.variables.insert(
                args[i].clone(),
                VarLoc::Stack((i as i64) - 4)
            );
        }
    }
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

// Returns the location of the final expression
fn gen_expr<'a>(c: &'a mut FunContext, expr: &'a Expr) -> &'a VarLoc {
    match expr {
        Expr::Int(value) => {
            println!("  movq ${}, %rax", value);
            &VarLoc::Register(Reg::Rax)
        },
        Expr::Id(name) => {
            match c.variables.get(name) {
                Some(location) => location,
                None => panic!("Variable {} not in scope", name),
            }
        },
        Expr::Operator(op, lhs, rhs) => {
            // This is pretty dumb for now
            // It should use registers instead of relying on the stack 100%
            println!("  pushq {}", gen_expr(c, lhs));
            let rhs_loc = gen_expr(c, rhs);

            match op {
                Op::Add => {
                    println!("  addq {}, (%rsp)", rhs_loc);
                    println!("  popq %rax");
                    &VarLoc::Register(Reg::Rax)
                },
                op => panic!("Operator {:?} not supported", op),
            }
        },
        // _ => panic!("Expr type {:?} not yet supported", expr)
    }
}

fn gen_return(c: &mut FunContext, expr: &Expr) {
    let loc = gen_expr(c, &expr);

    // If the location is already rax, we don't need to move!
    if loc != &VarLoc::Register(Reg::Rax) {
        println!("  movq {}, %rax", loc);
    }

    println!("  leave");
    println!("  retq");
}

// Returns true if the last statement is a return
fn gen_fun_body(c: &mut FunContext, body: &Statement) -> bool {
    match body {
        Statement::Return(expr) => {
            gen_return(c, expr);
            true
        },
        Statement::Block(statements) => {
            let mut trailing_ret = false;
            for statement in statements {
                trailing_ret = gen_fun_body(c, statement)
            }
            trailing_ret
        },
        _ => false,
    }
}

fn gen_fun(name: String, args: Vec<String>, body: Statement) {
    let mut c = FunContext {
        variables: HashMap::new(),
    };

    println!("{}:", name);
    // Save base pointer, since it's callee-saved
    println!("  pushq %rbp");
    println!("  movq %rsp, %rbp");
    // Prepare initial stack memory
    alloc_args(&mut c, &args);
    alloc_autos(&mut c, &body, 1 + std::cmp::min(6, args.len() as i64));

    let has_ret = gen_fun_body(&mut c, &body);

    if !has_ret {
        println!("  leave");
        println!("  retq");
    }

    println!("Entry variable allocation: {:?}:", c.variables);
}

pub fn generate(statements: Vec<RootStatement>) {
    for statement in statements {
        match statement {
            RootStatement::Function(name, args, body) => {
                gen_fun(name, args, body);
            },
            //other => println!("Can't compile this yet: {:?}", other),
        }
    }
}
