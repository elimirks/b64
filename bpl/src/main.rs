mod ast;
mod codegen;
mod memory;
mod parser;
mod tokenizer;

use std::env;
use std::fs;

use parser::parse;
use codegen::generate;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    generate(parse(contents));
}
