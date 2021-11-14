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
 * @param offset The 
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
fn gen_expr(c: &mut FunContext, expr: Expr) -> VarLoc {
    VarLoc::Register(Reg::Rax)
}

fn gen_return(c: &mut FunContext, expr: Expr) {
    // TODO: First generate the result of the expr ...
}

fn gen_fun_body(c: &mut FunContext, body: Statement) {
    match body {
        Statement::Return(expr) => gen_return(c, expr),
        _ => {},
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

    // TODO: <interesting stuff goes here>

    println!("  leave");
    println!("  retq");

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
