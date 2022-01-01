mod ast;
mod codegen;
mod memory;
mod parser;
mod tokenizer;
mod util;

use std::fs;
use std::io;
use std::env;
use std::process::{Command, Stdio};
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use parser::*;
use codegen::generate;

struct Opts {
    asm: bool,
    run: bool,
    output: Option<String>,
    inputs: Vec<String>,
}

// jemalloc performs a lot better in multithreaded applications
#[global_allocator]
static ALLOC: jemallocator::Jemalloc = jemallocator::Jemalloc;

fn main() {
    let opts = parse_opts();

    if opts.asm {
        let parse_result = parse_or_die(&opts.inputs);
        let mut stdout = io::stdout();
        generate(parse_result, &mut stdout);
    } else {
        let output_path = if opts.run {
            let pid = std::process::id();
            format!("/tmp/b64_{}.bin", pid)
        } else {
            match opts.output {
                Some(output) => output,
                None         => "a.out".to_string(),
            }
        };

        compile(&opts.inputs, &output_path);

        if opts.run {
            let prog_status = Command::new(&output_path)
                .status()
                .expect("Failed running program");

            fs::remove_file(output_path).unwrap();

            std::process::exit(match prog_status.code() {
                Some(code) => code,
                _          => 1,
            });
        }
    }
}

fn compile(input_paths: &Vec<String>, output_path: &String) {
    let mut hasher = DefaultHasher::new();
    input_paths.hash(&mut hasher);
    let run_hash = hasher.finish();

    let tmp_obj_path = format!("/tmp/b64_{}.o", run_hash);

    let mut as_process = Command::new("as")
        .stdin(Stdio::piped())
        .arg("-o")
        .arg(&tmp_obj_path)
        .spawn()
        .expect("Failed running the GNU Assembler");

    let parse_result = parse_or_die(&input_paths);
    // Stream the assembly code straight into GNU assembler
    generate(parse_result, &mut as_process.stdin.as_ref().unwrap());

    match as_process.wait() {
        Ok(status) => if !status.success() {
            let code = match status.code() {
                Some(code) => code,
                None       => 1,
            };
            std::process::exit(code);
        },
        Err(message) => {
            println!("Failed running GNU Assembler: {}", message);
            std::process::exit(1);
        },
    }

    let ld_status = Command::new("ld")
        .arg(&tmp_obj_path)
        .arg("-o")
        .arg(&output_path)
        .status()
        .expect("Failed running GNU Linker");

    if !ld_status.success() {
        let code = match ld_status.code() {
            Some(code) => code,
            None       => 1,
        };
        std::process::exit(code);
    } else {
        fs::remove_file(tmp_obj_path).unwrap();
    }
}

fn parse_opts() -> Opts {
    let args: Vec<String> = env::args().collect();
    let name = args[0].clone();

    let mut opts = Opts {
        asm: false,
        run: false,
        output: None,
        inputs: vec!(),
    };

    let mut i = 1;
    while i < args.len() {
        let arg: &String = &args[i];

        match arg.as_ref() {
            "-h" | "--help" => {
                print_usage(&name);
                std::process::exit(0);
            },
            "-s" => {
                opts.asm = true;
            },
            "-r" => {
                opts.run = true;
            },
            "-o" => {
                if i + 1 >= args.len() {
                    print_usage(&name);
                    std::process::exit(1);
                }
                i += 1;
                opts.output = Some(args[i].clone());
            },
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
    println!("    -s             Compile to ASM, not into a binary");
    println!("    -o <OUTPUT>    Write the output to the given path");
    println!("    -r             Directly run instead of saving the binary");
}

fn parse_or_die(inputs: &Vec<String>) -> ParseResult {
    let parse_result = parse_files(inputs);

    for err in &parse_result.errors {
        print_comp_error(&parse_result.file_paths, &err);
    }
    if !parse_result.errors.is_empty() {
        std::process::exit(1);
    }
    parse_result
}
