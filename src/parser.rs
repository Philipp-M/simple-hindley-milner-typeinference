use crate::ast::{Expr, Lit};
use chumsky::prelude::*;

fn parser() -> impl Parser<char, Expr, Error = Simple<char>> {
    let ident = text::ident().padded();

    recursive(|expr| {
        // 42
        let int = text::int(10).map(|s: String| Expr::Lit(Lit::Int(s.parse().unwrap()))).padded();
        // true
        let true_ = text::keyword("true").map(|_| Expr::Lit(Lit::Bool(true))).padded();
        // false
        let false_ = text::keyword("false").map(|_| Expr::Lit(Lit::Bool(false))).padded();
        // let <ident> = <expr> in <expr>
        let let_in = text::keyword("let")
            .padded()
            .ignore_then(ident)
            .then_ignore(just('='))
            .then(expr.clone())
            .then_ignore(text::keyword("in").padded())
            .then(expr.clone())
            .map(|((name, let_body), in_body)| {
                Expr::Let(name, Box::new(let_body), Box::new(in_body))
            });
        // \x -> x
        let lambda = just('\\')
            .padded()
            .ignore_then(ident)
            .then_ignore(just('-'))
            .then_ignore(just('>'))
            .then(expr.clone())
            .map(|(name, body)| Expr::Lam(name, Box::new(body)));
        // (<expr>)
        let paren_expr = expr.delimited_by(just('('), just(')'));
        let atom =
            paren_expr.or(let_in).or(lambda).or(int).or(true_).or(false_).or(ident.map(Expr::Var));
        // atom
        // TODO application makes 'let' keyword into Expr::Var for some reason
        // <expr> <expr>
        atom.repeated()
            .at_least(1)
            .map(|v| v.into_iter().reduce(|e1, e2| Expr::App(e1.into(), e2.into())).unwrap())
    })
}

#[test]
fn parses_literals() {
    assert_eq!(parser().parse("(123)"), Ok(Expr::Lit(Lit::Int(123))));
    assert_eq!(parser().parse("((true))"), Ok(Expr::Lit(Lit::Bool(true))));
    assert_eq!(parser().parse("false"), Ok(Expr::Lit(Lit::Bool(false))));
}

#[test]
fn parses_lambda() {
    assert_eq!(
        parser().parse("\\x -> x"),
        Ok(Expr::Lam("x".into(), Box::new(Expr::Var("x".into()))))
    );
    assert_eq!(
        parser().parse("\\hello -> \\world -> 42"),
        Ok(Expr::Lam(
            "hello".into(),
            Box::new(Expr::Lam("world".into(), Box::new(Expr::Lit(Lit::Int(42)))))
        ))
    );
}

#[test]
fn parses_let_in() {
    assert_eq!(
        parser().parse("let hello = true in 123"),
        Ok(Expr::Let(
            "hello".into(),
            Box::new(Expr::Lit(Lit::Bool(true))),
            Box::new(Expr::Lit(Lit::Int(123)))
        ))
    );
    assert_eq!(
        parser().parse("(let hello = true in hello)"),
        Ok(Expr::Let(
            "hello".into(),
            Box::new(Expr::Lit(Lit::Bool(true))),
            Box::new(Expr::Var("hello".into()))
        ))
    );
}

#[test]
fn parses_application() {
    let app = Ok(Expr::App(
        Box::new(Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::Var("hello".into())),
                Box::new(Expr::Lit(Lit::Int(1))),
            )),
            Box::new(Expr::Var("world".into())),
        )),
        Box::new(Expr::Lit(Lit::Bool(true))),
    ));

    assert_eq!(parser().parse("((hello 1) world) true"), app);
    assert_eq!(parser().parse("hello 1 world true"), app);
}

#[test]
fn parses_combined_expression() {
    assert_eq!(
        parser().parse("let id = \\x -> x in id 42"),
        Ok(Expr::Let(
            "id".into(),
            Box::new(Expr::Lam("x".into(), Box::new(Expr::Var("x".into())))),
            Box::new(Expr::App(
                Box::new(Expr::Var("id".into())),
                Box::new(Expr::Lit(Lit::Int(42)))
            )),
        ))
    );
}
