#![feature(iter_intersperse)]
mod ast;
mod parser;
use ast::Type;

fn main() {
    let ty = Type::Fun(
        Type::Fun(Type::Int.into(), Type::Var("a".into()).into()).into(),
        Type::Bool.into(),
    );
    println!("Hello, type: {}!", ty);
}
