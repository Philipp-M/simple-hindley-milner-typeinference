use std::fmt::Display;

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub enum Expr {
    Var(String),
    Lit(Lit),
    App(Box<Expr>, Box<Expr>),
    Lam(String, Box<Expr>),
    Let(String, Box<Expr>, Box<Expr>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub enum Lit {
    Int(i32),
    Bool(bool),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub enum Type {
    Int,
    Bool,
    Var(String),
    Fun(Box<Type>, Box<Type>),
}

#[derive(PartialEq, Eq, PartialOrd, Ord, Debug, Clone)]
pub struct Scheme {
    vars: Vec<String>,
    ty: Type,
}

impl Display for Type {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Type::Int => write!(f, "Int"),
            Type::Bool => write!(f, "Bool"),
            Type::Var(var) => write!(f, "{var}"),
            Type::Fun(ty1, ty2) if matches!(&**ty1, &Type::Fun(_, _)) => {
                write!(f, "({ty1}) -> {ty2}")
            }
            Type::Fun(ty1, ty2) => write!(f, "{ty1} -> {ty2}"),
        }
    }
}
impl Display for Scheme {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self.vars.as_slice() {
            [] => write!(f, "{}", self.ty),
            vars => {
                let vars = vars.iter().zip('a'..='z').collect::<Vec<_>>();
                let renamed_ty =
                    vars.iter().fold(self.ty.clone(), |t, vn| rename_var(&t, vn.0, vn.1));
                write!(
                    f,
                    "forall {}. {renamed_ty}",
                    vars.into_iter()
                        .unzip::<&String, char, Vec<_>, Vec<_>>()
                        .1
                        .iter()
                        .intersperse(&' ')
                        .collect::<String>()
                )
            }
        }
    }
}

fn rename_var(ty: &Type, old: &str, new: char) -> Type {
    use Type::*;
    match ty {
        Int => Int,
        Bool => Bool,
        Var(var) => Var(if var == old { new.to_string() } else { var.clone() }),
        Fun(ty1, ty2) => {
            Fun(Box::new(rename_var(ty1, old, new)), Box::new(rename_var(ty2, old, new)))
        }
    }
}
