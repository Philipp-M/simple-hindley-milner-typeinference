#![feature(iter_intersperse)]
mod ast;
mod parser;
use std::{env, fs};

use ast::Type;

fn main() {
    let ty = Type::Fun(
        Type::Fun(Type::Int.into(), Type::Var("a".into()).into()).into(),
        Type::Bool.into(),
    );
    println!("Hello, type: {}!", ty);

    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");
    println!("{:?}", parser::parse(&src));
}
