use std::env;
use std::fs;

mod ast;
use ast::*;

fn main() {
    let args: Vec<String> = env::args().collect();
    let filename = &args[1];

    let contents = fs::read_to_string(filename)
        .expect("Something went wrong reading the file");

    parse(contents);
}
