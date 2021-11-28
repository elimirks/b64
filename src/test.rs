use std::process::Command;
use std::fs;
use std::collections::hash_map::DefaultHasher;
use std::hash::{Hash, Hasher};

use crate::compile;

#[test]
fn test_fib_rec() {
    assert_eq!(55, run(vec!(
        "test/fib_rec.b".to_string(),
    )));
}

#[test]
fn test_fib_loop() {
    assert_eq!(144, run(vec!(
        "test/fib_loop.b".to_string(),
    )));
}

#[test]
fn test_fib_goto() {
    assert_eq!(144, run(vec!(
        "test/fib_goto.b".to_string(),
    )));
}

#[test]
fn test_dereference() {
    assert_eq!(7, run(vec!(
        "test/dereference.b".to_string(),
    )));
}

#[test]
fn test_extern() {
    assert_eq!(50, run(vec!(
        "test/extern.b".to_string(),
    )));
}

fn run(inputs: Vec<String>) -> i32 {
    let mut hasher = DefaultHasher::new();
    inputs.hash(&mut hasher);
    let run_hash = hasher.finish();

    let output_path = format!("/tmp/b64_test_{}.bin", run_hash);
    compile(&inputs, &output_path);

    let prog_status = Command::new(&output_path)
        .status()
        .expect("Failed running program");

    let code = match prog_status.code() {
        Some(code) => code,
        _          => 1,
    };
    fs::remove_file(output_path).unwrap();
    code
}
