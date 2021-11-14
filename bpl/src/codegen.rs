use std::collections::HashMap;

use crate::ast::*;

// Where variables are located
// In the future it should have Register(...) and Data(...)
#[derive(Debug)]
enum VarLoc {
    // Stack position relative to the return address
    // Negative values mean before the return address
    Stack(i64),
}

struct FunContext {
    variables: HashMap<String, VarLoc>,
    // Size of the stack, excluding things above (and including) return address
    stack_size: i64,
}

/**
 * Finds the amount of extra stack memory we need to allocate
 * @return A map of identifier to memory size (always 1 except for vectors)
 */
fn pre_alloc_autos(c: &mut FunContext, body: &Statement) {
    let mut stack = vec!(body);

    // DFS to find them all
    while !stack.is_empty() {
        match stack.pop().unwrap() {
            Statement::Auto(vars) => {
                for var in vars {
                    c.stack_size += 1;
                    // TODO: Die upon reassignment! Ban shadowing for now
                    c.variables.insert(
                        var.clone(),
                        VarLoc::Stack(c.stack_size)
                    );
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
}

fn pre_alloc_args(c: &mut FunContext, args: &Vec<String>) {
    for i in 0..args.len() {
        if i < 6 {
            c.stack_size += 1;
            c.variables.insert(
                args[i].clone(),
                VarLoc::Stack(c.stack_size)
            );
        } else {
            c.variables.insert(
                args[i].clone(),
                VarLoc::Stack(5 - i as i64)
            );
        }
    }
}

fn register_for_arg_num(num: usize) -> String {
    match num {
        0 => "rdi".to_string(),
        1 => "rsi".to_string(),
        2 => "rdx".to_string(),
        3 => "rcx".to_string(),
        4 => "r8".to_string(),
        5 => "r9".to_string(),
        _ => {
            panic!("arg num {} is not stored in a register", num)
        }
    }
}

fn gen_function(name: String, args: Vec<String>, body: Statement) {
    let mut c = FunContext {
        variables: HashMap::new(),
        stack_size: 0,
    };

    pre_alloc_args(&mut c, &args);
    pre_alloc_autos(&mut c, &body);

    println!("Entry variable allocation: {:?}:", c.variables);
    println!("Entry stack size: {}", c.stack_size);
    
    println!("{}:", name);

    // Allocate initial stack memory
    if c.stack_size > 0 {
        println!("  subq ${}, %rsp", c.stack_size * 8);
    }

    // Move first 6 args onto the stack
    for i in 0..std::cmp::min(6, args.len()) {
        let register = register_for_arg_num(i);
        let VarLoc::Stack(index) = c.variables.get(&args[i]).unwrap();
        println!("  movq %{} {}(%rsp)", register, 8 * (c.stack_size - index));
    }

    if c.stack_size > 0 {
        println!("  addq ${}, %rsp", 8 * c.stack_size);
    }

    println!("  retq");
}

pub fn generate(statements: Vec<RootStatement>) {
    for statement in statements {
        match statement {
            RootStatement::Function(name, args, body) => {
                gen_function(name, args, body);
            },
            //other => println!("Can't compile this yet: {:?}", other),
        }
    }
}
