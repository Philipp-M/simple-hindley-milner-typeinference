use crate::ast::{Expr, Lit};
use chumsky::prelude::*;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone, Hash)]
enum Token {
    Let,
    In,
    Eq,
    LParen,
    RParen,
    Arrow,
    Lambda,
    Bool(bool),
    Number(i32),
    Ident(String),
}

fn lexer() -> impl Parser<char, Vec<Token>, Error = Simple<char>> {
    let num = text::int(10)
        .chain::<char, _, _>(just('.').chain(text::digits(10)).or_not().flatten())
        .collect::<String>()
        .map(|n| Token::Number(n.parse().unwrap()));

    let ident = text::ident().map(|ident: String| match ident.as_str() {
        "in" => Token::In,
        "let" => Token::Let,
        "true" => Token::Bool(true),
        "false" => Token::Bool(false),
        _ => Token::Ident(ident),
    });

    let eq = just('=').map(|_| Token::Eq);
    let arrow = just("->").map(|_| Token::Arrow);
    let l_paren = just('(').map(|_| Token::LParen);
    let r_paren = just(')').map(|_| Token::RParen);
    let lambda = just('\\').map(|_| Token::Lambda);

    // A single token can be one of the above
    let token = num.or(eq).or(arrow).or(l_paren).or(r_paren).or(arrow).or(lambda).or(ident);

    let comment = just("#").then(take_until(just('\n'))).padded();

    token.padded_by(comment.repeated()).padded().repeated()
}

fn parser() -> impl Parser<Token, Expr, Error = Simple<Token>> + Clone {
    recursive(|expr| {
        // 42
        let lit = select! {
            Token::Bool(x) => Expr::Lit(Lit::Bool(x)),
            Token::Number(n) => Expr::Lit(Lit::Int(n)),
        };

        let ident = select! { Token::Ident(ident) => ident };
        let let_in = just(Token::Let)
            .ignore_then(ident)
            .then_ignore(just(Token::Eq))
            .then(expr.clone())
            .then_ignore(just(Token::In))
            .then(expr.clone())
            .map(|((name, let_body), in_body)| {
                Expr::Let(name, Box::new(let_body), Box::new(in_body))
            });
        // \x -> x
        let lambda = just(Token::Lambda)
            .ignore_then(ident)
            .then_ignore(just(Token::Arrow))
            .then(expr.clone())
            .map(|(name, body)| Expr::Lam(name, Box::new(body)));
        // (<expr>)
        let paren_expr = expr.delimited_by(just(Token::LParen), just(Token::RParen));
        let atom = paren_expr.or(let_in).or(lit).or(lambda).or(ident.map(Expr::Var));
        // <expr> <expr>
        atom.clone().then(atom.repeated()).foldl(|e1, e2| Expr::App(e1.into(), e2.into()))
    })
}

#[test]
fn parses_literals() {
    assert_eq!(parser().parse(lexer().parse("(123)").unwrap()), Ok(Expr::Lit(Lit::Int(123))));
    assert_eq!(parser().parse(lexer().parse("((true))").unwrap()), Ok(Expr::Lit(Lit::Bool(true))));
    assert_eq!(parser().parse(lexer().parse("false").unwrap()), Ok(Expr::Lit(Lit::Bool(false))));
}

#[test]
fn parses_lambda() {
    assert_eq!(
        parser().parse(lexer().parse("\\x -> x").unwrap()),
        Ok(Expr::Lam("x".into(), Box::new(Expr::Var("x".into()))))
    );
    assert_eq!(
        parser().parse(lexer().parse("\\hello -> \\world -> 42").unwrap()),
        Ok(Expr::Lam(
            "hello".into(),
            Box::new(Expr::Lam("world".into(), Box::new(Expr::Lit(Lit::Int(42)))))
        ))
    );
}

#[test]
fn parses_let_in() {
    assert_eq!(
        parser().parse(lexer().parse("let hello = 8 in 9").unwrap()),
        Ok(Expr::Let(
            "hello".into(),
            Box::new(Expr::Lit(Lit::Int(8))),
            Box::new(Expr::Lit(Lit::Int(9)))
        ))
    );
    assert_eq!(
        parser().parse(lexer().parse("(let hello = true in hello)").unwrap()),
        Ok(Expr::Let(
            "hello".into(),
            Box::new(Expr::Lit(Lit::Bool(true))),
            Box::new(Expr::Var("hello".into()))
        ))
    );
}

#[test]
fn parses_application() {
    let app = Expr::App(
        Box::new(Expr::App(
            Box::new(Expr::App(
                Box::new(Expr::Var("hello".into())),
                Box::new(Expr::Lit(Lit::Int(1))),
            )),
            Box::new(Expr::Var("world".into())),
        )),
        Box::new(Expr::Lit(Lit::Bool(true))),
    );

    assert_eq!(parser().parse(lexer().parse("((hello 1) world) true").unwrap()).unwrap(), app);
    assert_eq!(parser().parse(lexer().parse("hello 1 world true").unwrap()).unwrap(), app);
}

#[test]
fn parses_combined_expression() {
    assert_eq!(
        parser().parse(lexer().parse("let id = \\x -> x in id 42").unwrap()),
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
