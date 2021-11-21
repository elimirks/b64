mod ast;
mod codegen;
mod memory;
mod parser;
mod tokenizer;

use std::fs;
use std::io;
use std::process::{Command, Stdio};

use clap::Parser;

use parser::parse;
use codegen::generate;

#[derive(Parser)]
struct Opts {
    /// Input file to compile
    inputs: Vec<String>,
    /// Location to write the compiled binary to
    #[clap(short, long, default_value = "a.out")]
    output: String,
    /// Write output assembly to stdout instead of fully compiling
    #[clap(short, long)]
    no_bin: bool,
    /// Run the program immediately, not saving the binary
    #[clap(short, long)]
    run: bool,
}

fn main() {
    let opts: Opts = Opts::parse();

    let mut root_statements = vec!();

    for input in opts.inputs {
        let contents = fs::read_to_string(input)
            .expect("Something went wrong reading the file");

        root_statements.append(&mut parse(contents));
    }

    if opts.no_bin {
        let mut stdout = io::stdout();
        generate(root_statements, &mut stdout);
    } else {
        let tmp_obj_path = "/tmp/b64.o";

        let mut as_process = Command::new("as")
            .stdin(Stdio::piped())
            .arg("-o")
            .arg(tmp_obj_path)
            .spawn()
            .expect("Failed running the GNU Assembler");

        // Stream the assembly code straight into GNU assembler
        generate(root_statements, &mut as_process.stdin.as_ref().unwrap());

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
            opts.output
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
