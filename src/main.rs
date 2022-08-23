#![feature(iter_intersperse)]
mod ast;
mod parser;
mod typechecker;

use std::{env, fs};
use typechecker::test_type_inference;

fn main() {
    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");
    let expr = parser::parse(&src);
    println!("The expression:\n");
    println!("{expr}");
    println!("\nhas type:\n");
    println!("{}", test_type_inference(&expr));
}
