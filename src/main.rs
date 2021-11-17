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
    no_bin: bool
}

fn main() {
    let opts: Opts = Opts::parse();

    let contents = fs::read_to_string(opts.input)
        .expect("Something went wrong reading the file");

    if opts.no_bin {
        let mut stdout = io::stdout();
        generate(parse(contents), &mut stdout);
    } else {
        let tmp_file_path = "/tmp/bpl.s";
        let mut tmp_file = fs::File::create(tmp_file_path)
            .expect("Couldn't create temp asm file");

        generate(parse(contents), &mut tmp_file);

        // TODO: Output stderr if appropriate
        Command::new("gcc")
            .arg("-nostdlib")
            .arg(tmp_file_path)
            .arg("-o")
            .arg(opts.output)
            .output()
            .expect("Failed running GCC");

        fs::remove_file(tmp_file_path)
            .expect("Failed removing temp file");
    }
}
