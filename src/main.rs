mod ast;
mod codegen;
mod memory;
mod parser;
mod tokenizer;
mod util;

use std::collections::hash_map::DefaultHasher;
use std::env;
use std::fs;
use std::fs::File;
use std::hash::{Hash, Hasher};
use std::io;
use std::process::{Command, Stdio};

use codegen::generate;
use parser::*;

struct Opts {
    run: bool,
    output: Option<String>,
    inputs: Vec<String>,
    args: Vec<String>,
}

// jemalloc performs a lot better in multithreaded applications
#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

fn main() {
    let opts = parse_opts();

    let output_path = if opts.run {
        let pid = std::process::id();
        format!("/tmp/b64_{}.bin", pid)
    } else {
        match opts.output {
            Some(output) => output,
            None => "a.out".to_string(),
        }
    };

    compile(&opts.inputs, &output_path);

    if opts.run {
        let prog_status = Command::new(&output_path)
            .args(opts.args)
            .status()
            .expect("Failed running program");

        fs::remove_file(output_path).unwrap();

        std::process::exit(match prog_status.code() {
            Some(code) => code,
            _ => 1,
        });
    }
}

fn compile(input_paths: &Vec<String>, output_path: &String) {
    let parse_result = parse_or_die(input_paths);
    // Stream the assembly code straight into GNU assembler
    generate(parse_result, &mut File::create(output_path).unwrap());
}

fn parse_opts() -> Opts {
    let args: Vec<String> = env::args().collect();
    let name = args[0].clone();

    let mut opts = Opts {
        run: false,
        output: None,
        inputs: vec![],
        args: vec![],
    };

    let mut i = 1;
    let mut pass_through = false;
    while i < args.len() {
        let arg: &String = &args[i];

        if pass_through {
            opts.args.push(arg.to_string());
            i += 1;
            continue;
        }

        match arg.as_ref() {
            "-h" | "--help" => {
                print_usage(&name);
                std::process::exit(0);
            }
            "-r" => {
                opts.run = true;
            }
            "-o" => {
                if i + 1 >= args.len() {
                    print_usage(&name);
                    std::process::exit(1);
                }
                i += 1;
                opts.output = Some(args[i].clone());
            }
            "--" => pass_through = true,
            input => opts.inputs.push(input.to_string()),
        }

        i += 1;
    }
    if opts.inputs.is_empty() {
        print_usage(&name);
        std::process::exit(1);
    }
    opts
}

fn print_usage(name: &String) {
    println!("USAGE:\n    {} [OPTIONS] [INPUTS]", name);
    println!("OPTIONS:");
    println!("    -h, --help     Print this message");
    println!("    -o <OUTPUT>    Write the output to the given path");
    println!("    -r             Directly run instead of saving the binary");
    println!("    --             Any args after '--' will pass through");
}

fn parse_or_die(inputs: &Vec<String>) -> ParseResult {
    let parse_result = parse_files(inputs);

    for err in &parse_result.errors {
        print_comp_error(&parse_result.file_paths, err);
    }
    if !parse_result.errors.is_empty() {
        std::process::exit(1);
    }
    parse_result
}
