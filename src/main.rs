mod ast;
mod codegen;
mod memory;
mod parser;
mod tokenizer;

use std::fs;
use std::io;
use std::env;
use std::process::{Command, Stdio};

use parser::*;
use codegen::generate;

struct Opts {
    asm: bool,
    run: bool,
    output: Option<String>,
    inputs: Vec<String>,
}

fn main() {
    let opts = parse_opts();
    let parse_result = parse_or_die(&opts.inputs);

    if opts.asm {
        let mut stdout = io::stdout();
        generate(&parse_result, &mut stdout);
    } else {
        let tmp_obj_path = "/tmp/b64.o";

        let mut as_process = Command::new("as")
            .stdin(Stdio::piped())
            .arg("-o")
            .arg(tmp_obj_path)
            .spawn()
            .expect("Failed running the GNU Assembler");

        // Stream the assembly code straight into GNU assembler
        generate(&parse_result, &mut as_process.stdin.as_ref().unwrap());

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

        let output_path = if opts.run {
            "/tmp/b64.bin".to_string()
        } else {
            match opts.output {
                Some(output) => output,
                None         => "a.out".to_string(),
            }
        };

        let ld_status = Command::new("ld")
            .arg(tmp_obj_path)
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
        }

        fs::remove_file(tmp_obj_path).unwrap();

        if opts.run {
            let prog_status = Command::new(&output_path)
                .status()
                .expect("Failed running program");

            fs::remove_file(output_path).unwrap();
            std::process::exit(prog_status.code().unwrap());
        }
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
    if let Some(ref err) = parse_result.error {
        print_comp_error(&parse_result, &err);
        std::process::exit(1);
    }
    parse_result
}
