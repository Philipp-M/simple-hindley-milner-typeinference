#![feature(iter_intersperse)]
mod ast;
mod parser;
mod typechecker;

use std::{env, fs, io};
use typechecker::test_type_inference;

use crate::parser::parse;

fn main() -> Result<(), io::Error> {
    fn print_expression_and_type(src: &str) {
        let expr = parse(src);
        println!("\nThe expression:");
        println!("{expr}");
        println!("\nhas type:");
        println!("{}", test_type_inference(&expr));
    }
    if env::args().len() > 1 {
        let src = fs::read_to_string(env::args().nth(1).expect("Expected file argument"))
            .expect("Failed to read file");
        print_expression_and_type(&src);
    } else {
        loop {
            let mut line = String::new();
            io::stdin().read_line(&mut line)?;
            if line.trim().is_empty() {
                return Ok(());
            }
            print_expression_and_type(&line);
        }
    }
    Ok(())
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
        // recursion with fixed point operator (works without modifying original hindley milner)
        assert_eq!(
            "Int -> Int",
            ty("let factorial = fix (\\fact -> \\n -> if (gte n 2) (mul n (fact (add n -1))) 1) in factorial")
        );
        // recursion without fixed point operator
        assert_eq!(
            "Int -> Int",
            ty("let fact = \\n -> if (gte n 2) (mul n (fact (add n -1))) 1 in fact")
        );
        assert_eq!(ty("fix"), ty("let fx = fix \\fix -> (\\f -> f (fix f)) in fx"));
        assert_eq!(ty("fix"), ty("let fix = \\f -> f (fix f) in fix"));
        // mutual recursion
        assert_eq!(
            "Int -> Bool",
            ty("
            let even = \\n -> if (gte 1 n) (odd (add -1 n)) true,
                odd = \\n -> if (gte 1 n) (odd (add -1 n)) false in even
        ")
        );
        assert_eq!(
            "Int -> Int",
            ty("
            let f = \\n -> h 1,
                h = \\x -> 0,
                g = \\n -> h 0,
            in h
        ")
        );
        assert_eq!(
            "forall a. a -> Int",
            ty("
            let f = \\n -> h 1,
                h = \\x -> 0,
                g = \\n -> h 0,
            in g
        ")
        );
        // recursion in hindley milner inference generally fails to find bottom
        assert_eq!("forall a. a", ty("let f = h, h = f in f"));
        assert_eq!(
            "Int",
            ty("
            let a = b,
                e = f,
                b = c,
                f = 0,
                d = e,
                c = d,
            in add a d
        ")
        );
    }
}
