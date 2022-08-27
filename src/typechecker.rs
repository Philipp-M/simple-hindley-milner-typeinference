use std::{
    collections::{BTreeMap, BTreeSet},
    sync::atomic::{AtomicU64, Ordering},
};

use crate::ast::{Expr, Lit, Scheme, Type};

static COUNTER: AtomicU64 = AtomicU64::new(0);

type Substitution = BTreeMap<String, Type>;

type Context = BTreeMap<String, Scheme>;

fn empty_subst() -> Substitution {
    Substitution::new()
}

/// substitute subst in ty
///
/// example:
/// subst = { "a" => Int, "b" => Int -> Int }
/// ty = "a" -> "b" -> "a" -> "c"
/// apply_subst(subst, ty) = Int -> (Int -> Int) -> Int -> "c"
fn apply_subst(subst: &Substitution, ty: &Type) -> Type {
    match ty {
        Type::Var(var) => subst.get(var).unwrap_or(ty).clone(),
        Type::Fun(arg, res) => {
            Type::Fun(apply_subst(subst, arg).into(), apply_subst(subst, res).into())
        }
        Type::Int => Type::Int,
        Type::Bool => Type::Bool,
    }
}

/// substitute subst in scheme
///
/// example:
/// subst = { "a" => Int -> Bool, "b" => Bool }
/// scheme = ∀ "a". ("a" -> "b" -> "b"),
/// apply_subst_scheme(subst, scheme) = ∀ "a". ("a" -> Bool -> Bool)
fn apply_subst_scheme(subst: &Substitution, mut scheme: Scheme) -> Scheme {
    // The fold takes care of bound vars in scheme.ty
    let subst_without_bound_vars = scheme.vars.iter().rfold(subst.clone(), |mut subst, var| {
        subst.remove(var);
        subst
    });
    scheme.ty = apply_subst(&subst_without_bound_vars, &scheme.ty);
    scheme
}

/// substitute all vars in s2 with s1 and extends s1 with this substitution (overwriting substitutions in s1 if they exist)
///
/// example:
/// s1 = { "a" => Int, "c" => Int -> Bool }
/// s2 = { "a" => Bool, "b" => Int -> "c" }
/// compose_subst(s1, s2) = { "a" => Bool, "b" => Int -> Int -> Bool, "c" => Int -> Bool }
fn compose_subst(s1: &Substitution, s2: &Substitution) -> Substitution {
    let substituted =
        s2.clone().into_iter().map(|(k, v)| (k, apply_subst(s1, &v))).collect::<BTreeMap<_, _>>();
    let mut composed = s1.clone();
    composed.extend(substituted);
    composed
}

/// creates a new unique free type var
///
/// example: Var("%42")
fn new_ty_var() -> Type {
    let count = COUNTER.fetch_add(1, Ordering::Relaxed);
    Type::Var(format!("%{count}"))
}

/// get all vars in type
///
/// example:
/// ty = Int -> "a" -> Bool -> "b" -> "a"
/// free_type_vars(ty) = { "a", "b" }
fn free_type_vars(ty: &Type) -> BTreeSet<String> {
    match ty {
        Type::Var(var) => BTreeSet::from([var.clone()]),
        Type::Fun(t1, t2) => {
            let mut s = free_type_vars(t1);
            s.extend(free_type_vars(t2));
            s
        }
        _ => BTreeSet::new(),
    }
}

/// get all free vars in scheme
///
/// example:
/// scheme = ∀ "a". (Int -> "a" -> Bool -> "b" -> "c")
/// free_type_vars_scheme(scheme) = { "b", "c" }
fn free_type_vars_scheme(scheme: Scheme) -> BTreeSet<String> {
    let s1 = free_type_vars(&scheme.ty);
    s1.difference(&BTreeSet::from_iter(scheme.vars.iter().cloned())).map(Into::into).collect()
}

/// if ty1 and ty2 unify unify(ty1, ty2) returns substitution S such that apply_subst(S, ty1) == apply_subst(S, ty2)
/// otherwise it (currently) panics
fn unify(ty1: &Type, ty2: &Type) -> Substitution {
    match (ty1, ty2) {
        // primitive types
        (Type::Int, Type::Int) | (Type::Bool, Type::Bool) => empty_subst(),
        // same variables
        (Type::Var(u), Type::Var(v)) if u == v => empty_subst(),
        // t contains var v (no unification, recursive var)
        (Type::Var(v), t) | (t, Type::Var(v)) if free_type_vars(t).contains(v) => {
            panic!("occurs check failed: type {t} contains {v}")
        }
        // v can be substituted by t
        (Type::Var(v), t) | (t, Type::Var(v)) => BTreeMap::from([(v.clone(), t.clone())]),
        // example:
        // t1: Var("a") -> Var("b"), t2: Int -> Bool
        // unify(t1, t2) = { "a" => Int, "b" => Bool }
        (Type::Fun(l1, r1), Type::Fun(l2, r2)) => {
            let s1 = unify(l1, l2);
            let s2 = unify(&apply_subst(&s1, r1), &apply_subst(&s1, r2));
            compose_subst(&s2, &s1)
        }
        (t1, t2) => panic!("types do not unify: {t1} vs. {t2}"),
    }
}

/// substitute subst in context
///
/// example:
/// subst = { "a" => Int -> Bool, "b" => Bool }
/// context = { f => ∀ "a". ("a" -> "b" -> "b"), id = ∀ "a". ("a" -> "a") },
/// apply_subst_ctx(subst, context) = { f => ∀ "a". ("a" -> Bool -> "b"), id = ∀ "a". ("a" -> "a") }
fn apply_subst_ctx(subst: &Substitution, context: Context) -> Context {
    context.into_iter().map(|(k, v)| (k, apply_subst_scheme(subst, v))).collect()
}

/// get all free vars in context
///
/// example:
/// context = { f => ∀ "a". ("a" -> "b" -> "b"), id = ∀ "a". ("a" -> "a") },
/// free_type_vars_scheme(scheme) = { "b" }
fn free_type_vars_ctx(context: Context) -> BTreeSet<String> {
    context
        .into_values()
        .map(free_type_vars_scheme)
        .reduce(|mut b, s| {
            b.extend(s);
            b
        })
        .unwrap_or_default()
}

/// create a forall binding for all free vars in ty, which are not free (or non-existent) in context
///
/// example:
/// context = { f => ∀ "a". ("a" -> "b" -> "b"), id = ∀ "a". ("a" -> "a") },
/// ty = ∀ "a". ("a" -> "b" -> "c")
/// generalize(context, ty) => ∀ "a" "c". ("a" -> "b" -> "c")
///
fn generalize(context: Context, ty: Type) -> Scheme {
    let vars = free_type_vars(&ty).difference(&free_type_vars_ctx(context)).cloned().collect();
    Scheme { vars, ty }
}

/// replaces bound vars in scheme with new free vars
///
/// example:
///
/// scheme = ∀ "a". ("a" -> "b" -> "b"),
/// instantiate(scheme) = "%1" -> "b" -> "b"
fn instantiate(scheme: Scheme) -> Type {
    apply_subst(&scheme.vars.into_iter().map(|v| (v, new_ty_var())).collect(), &scheme.ty)
}

fn infer_literal(lit: &Lit) -> (Substitution, Type) {
    match lit {
        Lit::Int(_) => (empty_subst(), Type::Int),
        Lit::Bool(_) => (empty_subst(), Type::Bool),
    }
}

/// Algorithm W
fn infer(ctx: &Context, expr: &Expr) -> (Substitution, Type) {
    match expr {
        Expr::Var(var) => (
            empty_subst(),
            // TODO handle errors correctly, preferably with spans to the original source
            instantiate(ctx.get(var).unwrap_or_else(|| panic!("unbound variable: {var}")).clone()),
        ),
        Expr::Lit(lit) => infer_literal(lit),
        Expr::App(fun, arg) => {
            let ty_res = new_ty_var();
            let (s1, ty_fun) = infer(ctx, fun);
            let tmp_ctx = &apply_subst_ctx(&s1, ctx.clone());
            let (s2, ty_arg) = infer(tmp_ctx, arg);
            let s3 =
                unify(&apply_subst(&s2, &ty_fun), &Type::Fun(ty_arg.into(), ty_res.clone().into()));
            (compose_subst(&s3, &compose_subst(&s2, &s1)), apply_subst(&s3, &ty_res))
        }
        Expr::Lam(binder, body) => {
            let ty_binder = new_ty_var();
            let mut tmp_ctx = ctx.clone();
            tmp_ctx.insert(binder.clone(), Scheme { vars: vec![], ty: ty_binder.clone() });
            let (s1, ty_body) = infer(&tmp_ctx, body);
            let ty_fun = Type::Fun(apply_subst(&s1, &ty_binder).into(), ty_body.into());
            (s1, ty_fun)
        }
        Expr::Let(binder, binding, body) => {
            let (s1, ty_binder) = infer(ctx, binding);
            let scheme = generalize(apply_subst_ctx(&s1, ctx.clone()), ty_binder);
            let mut tmp_ctx = ctx.clone();
            tmp_ctx.insert(binder.clone(), scheme);
            let (s2, ty_body) = infer(&apply_subst_ctx(&s1, tmp_ctx.clone()), body);
            (compose_subst(&s2, &s1), ty_body)
        }
    }
}

/// infer type with context of expression, can still contain free vars
pub fn type_inference(ctx: &Context, expr: &Expr) -> Type {
    let (s, t) = infer(ctx, expr);
    apply_subst(&s, &t)
}

/// Some predefined functions creating a context:
/// {
///   "identity" => ∀ "a". ("a" -> "a"),
///   "const" => ∀ "a" "b". ("a" -> "b" -> "a"),
///   "add" => Int -> Int -> Int,
///   "gte" => Int -> Int -> Bool,
///   "if" => ∀ "a". (Bool -> "a" -> "a")
/// }
pub fn primitives() -> Context {
    use Type::*;
    let mut ctx = Context::new();
    ctx.insert(
        "identity".into(),
        Scheme { vars: vec!["a".into()], ty: Fun(Var("a".into()).into(), Var("a".into()).into()) },
    );
    ctx.insert(
        "const".into(),
        Scheme {
            vars: vec!["a".into(), "b".into()],
            ty: Fun(
                Var("a".into()).into(),
                Fun(Var("b".into()).into(), Var("a".into()).into()).into(),
            ),
        },
    );
    ctx.insert(
        "add".into(),
        Scheme { vars: vec![], ty: Fun(Int.into(), Fun(Int.into(), Int.into()).into()) },
    );
    ctx.insert(
        "gte".into(),
        Scheme { vars: vec![], ty: Fun(Int.into(), Fun(Int.into(), Bool.into()).into()) },
    );
    ctx.insert(
        "if".into(),
        Scheme {
            vars: vec!["a".into()],
            ty: Fun(
                Bool.into(),
                Fun(
                    Var("a".into()).into(),
                    Fun(Var("a".into()).into(), Var("a".into()).into()).into(),
                )
                .into(),
            ),
        },
    );
    ctx
}

/// main type check function,
/// returns the most general type for the given expression (using the context given by primitives),
/// or panics otherwise (type error)
pub fn test_type_inference(expr: &Expr) -> Scheme {
    generalize(BTreeMap::new(), type_inference(&primitives(), expr))
}
