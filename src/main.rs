#![feature(iter_intersperse)]
mod ast;
mod parser;
mod typechecker;

use std::{env, fs};
use typechecker::test_type_inference;

use crate::parser::parse;

fn main() {
    let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
        .expect("Failed to read file");
    let expr = parse(&src);
    println!("The expression:\n");
    println!("{expr}");
    println!("\nhas type:\n");
    println!("{}", test_type_inference(&expr));
}

#[cfg(test)]
mod test {
    use super::*;

    fn ty(source: &str) -> String {
        let scheme = test_type_inference(&parse(source));
        format!("{scheme}")
    }

    #[cfg(test)]
    #[test]
    fn infers() {
        assert_eq!("forall a. a -> a", ty("\\x -> x"));
        assert_eq!("forall a. a -> Int", ty("\\x -> 42"));
        assert_eq!("Int -> Int -> Int", ty("(\\x -> add) true"));
        assert_eq!("Int -> Int -> Bool", ty("gte"));
        assert_eq!("Bool", ty("if (gte 1 0) true false"));
        // shadowing
        assert_eq!("forall a. a -> Int -> Int -> Bool", ty("let add = \\a -> \\b -> gte b in add"));
        assert_eq!("forall a. a -> a", ty("let x = \\a -> a in x"));
        assert_eq!("Int", ty("add (add 8 8) 9"));
        assert_eq!("Int", ty("let x = \\a -> add a a in x 8"));
        assert_eq!("Int -> Int -> Int", ty("let id = \\a -> a in id id id add"));
        assert_eq!(
            "forall a b. a -> b -> Int -> Int -> Int",
            ty("let x = \\x -> \\b -> \\c -> identity x in let y = x add in y")
        );
    }
}
