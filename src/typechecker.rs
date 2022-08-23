use std::{
    collections::{HashMap, HashSet},
    sync::atomic::{AtomicU64, Ordering},
};

use crate::ast::{Expr, Lit, Scheme, Type};

static COUNTER: AtomicU64 = AtomicU64::new(0);

type Substitution = HashMap<String, Type>;

type Context = HashMap<String, Scheme>;

fn empty_subst() -> Substitution {
    Substitution::new()
}

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

fn apply_subst_scheme(subst: &Substitution, mut scheme: Scheme) -> Scheme {
    // The fold takes care of name shadowing
    let shadowed_subst = scheme.vars.iter().rfold(subst.clone(), |mut subst, var| {
        subst.remove(var);
        subst
    });
    scheme.ty = apply_subst(&shadowed_subst, &scheme.ty);
    scheme
}

fn compose_subst(s1: &Substitution, s2: &Substitution) -> Substitution {
    let substituted =
        s2.clone().into_iter().map(|(k, v)| (k, apply_subst(s1, &v))).collect::<HashMap<_, _>>();
    let mut composed = s1.clone();
    composed.extend(substituted);
    composed
}

fn new_ty_var() -> Type {
    let count = COUNTER.fetch_add(1, Ordering::Relaxed);
    Type::Var(format!("%{count}"))
}

fn free_type_vars(ty: &Type) -> HashSet<String> {
    match ty {
        Type::Var(var) => HashSet::from([var.clone()]),
        Type::Fun(t1, t2) => {
            let mut s = free_type_vars(t1);
            s.extend(free_type_vars(t2));
            s
        }
        _ => HashSet::new(),
    }
}

fn free_type_vars_scheme(scheme: Scheme) -> HashSet<String> {
    let s1 = free_type_vars(&scheme.ty);
    s1.difference(&HashSet::from_iter(scheme.vars.iter().cloned())).map(Into::into).collect()
}

fn var_bind(var: String, ty: &Type) -> Substitution {
    if free_type_vars(ty).contains(&var) {
        panic!("occurs check failed");
    } else if ty == &Type::Var(var.clone()) {
        empty_subst()
    } else {
        HashMap::from([(var, ty.clone())])
    }
}

fn unify(ty1: &Type, ty2: &Type) -> Substitution {
    match (ty1, ty2) {
        (Type::Int, Type::Int) => empty_subst(),
        (Type::Bool, Type::Bool) => empty_subst(),
        (Type::Var(u), t) | (t, Type::Var(u)) => var_bind(u.clone(), t),
        (Type::Fun(l1, r1), Type::Fun(l2, r2)) => {
            let s1 = unify(l1, l2);
            let s2 = unify(&apply_subst(&s1, r1), &apply_subst(&s1, r2));
            compose_subst(&s2, &s1)
        }
        (t1, t2) => panic!("types do not unify: {t1} vs. {t2}"),
    }
}

fn apply_subst_ctx(subst: &Substitution, context: Context) -> Context {
    context.into_iter().map(|(k, v)| (k, apply_subst_scheme(subst, v))).collect()
}

fn free_type_vars_ctx(context: Context) -> HashSet<String> {
    context
        .into_values()
        .map(free_type_vars_scheme)
        .reduce(|mut b, s| {
            b.extend(s);
            b
        })
        .unwrap_or_default()
}

fn generalize(context: Context, ty: Type) -> Scheme {
    let vars = free_type_vars(&ty).difference(&free_type_vars_ctx(context)).cloned().collect();
    Scheme { vars, ty }
}

fn instantiate(scheme: Scheme) -> Type {
    apply_subst(&scheme.vars.into_iter().map(|v| (v, new_ty_var())).collect(), &scheme.ty)
}

fn infer_literal(lit: &Lit) -> (Substitution, Type) {
    match lit {
        Lit::Int(_) => (empty_subst(), Type::Int),
        Lit::Bool(_) => (empty_subst(), Type::Bool),
    }
}

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
            let (s2, ty_arg) = infer(&apply_subst_ctx(&s1, ctx.clone()), arg);
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
            let scheme = Scheme { vars: vec![], ty: apply_subst(&s1, &ty_binder) };
            let mut tmp_ctx = ctx.clone();
            tmp_ctx.insert(binder.clone(), scheme);
            let (s2, ty_body) = infer(&apply_subst_ctx(&s1, tmp_ctx), body);
            (compose_subst(&s2, &s1), ty_body)
        }
    }
}

pub fn type_inference(ctx: &Context, expr: &Expr) -> Type {
    let (s, t) = infer(ctx, expr);
    apply_subst(&s, &t)
}

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

pub fn test_type_inference(expr: &Expr) -> Scheme {
    generalize(HashMap::new(), type_inference(&primitives(), expr))
}
