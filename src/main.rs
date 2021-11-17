mod ast;
mod codegen;
mod memory;
mod parser;
mod tokenizer;

use std::fs;
use std::io;
use std::process::Command;

use clap::Parser;

use parser::parse;
use codegen::generate;

#[derive(Parser)]
struct Opts {
    /// Input file to compile
    input: String,
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

    let contents = fs::read_to_string(opts.input)
        .expect("Something went wrong reading the file");

    if opts.no_bin {
        let mut stdout = io::stdout();
        generate(parse(contents), &mut stdout);
    } else {
        let tmp_file_path = "/tmp/b64.s";
        let mut tmp_file = fs::File::create(tmp_file_path)
            .expect("Couldn't create temp asm file");

        generate(parse(contents), &mut tmp_file);

        let output_path = if opts.run {
            "/tmp/b64.bin".to_string()
        } else {
            opts.output
        };

        let gcc_status = Command::new("gcc")
            .arg("-nostdlib")
            .arg(tmp_file_path)
            .arg("-o")
            .arg(&output_path)
            .status()
            .expect("Failed running GCC");

        fs::remove_file(tmp_file_path).unwrap();

        if !gcc_status.success() {
            std::process::exit(gcc_status.code().unwrap());
        }

        if opts.run {
            let prog_status = Command::new(&output_path)
                .status()
                .expect("Failed running program");

            fs::remove_file(output_path).unwrap();
            std::process::exit(prog_status.code().unwrap());
        }
    }
}
