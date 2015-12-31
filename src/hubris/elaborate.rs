use ast;
use core;

use std::path::Path;

pub fn elaborate_module<P: AsRef<Path>>(path: P, module: ast::Module) -> core::Module {
    let name = elaborate_name(module.name);
    let defs = module.defs
                     .into_iter()
                     .filter_map(elaborate_def)
                     .collect();

    core::Module {
        file_name: path.as_ref().to_owned(),
        name: name,
        defs: defs,
    }
}

pub fn elaborate_def(def: ast::Definition) -> Option<core::Definition> {
    match def {
        ast::Definition::Data(d) => Some(core::Definition::Data(elaborate_data(d))),
        ast::Definition::Fn(f) => Some(core::Definition::Fn(elaborate_fn(f))),
        ast::Definition::Extern(e) => Some(core::Definition::Extern(elaborate_extern(e))),
        ast::Definition::Comment(_) => None
    }
}

fn elaborate_data(data: ast::Data) -> core::Data {
    core::Data {
        span: data.span,
        name: elaborate_name(data.name),
        ty: elaborate_term(data.ty),
        ctors: data.ctors
                   .into_iter()
                   .map(|(k, v)| (elaborate_name(k), elaborate_term(v)))
                   .collect()
    }
}

fn elaborate_fn(fun: ast::Function) -> core::Function {
    core::Function {
        name: elaborate_name(fun.name),
        args: fun.args
                 .into_iter()
                 .map(|(k, v)| (elaborate_name(k), elaborate_term(v)))
                 .collect(),
        ty: elaborate_term(fun.ty),
        body: elaborate_term(fun.body),
    }
}

fn elaborate_extern(ext: ast::Extern) -> core::Extern {
    let ast::Extern { span, name, term } = ext;
    core::Extern {
        span: span,
        name: elaborate_name(name),
        term: elaborate_term(term),
    }
}

fn elaborate_term(term: ast::Term) -> core::Term {
    match term {
        ast::Term::Literal { span, lit } => core::Term::Literal {
            span: span,
            lit: elaborate_literal(lit),
        },
        ast::Term::Var { name, .. } => core::Term::Var {
            name: elaborate_name(name)
        },
        ast::Term::Match { span, scrutinee, cases } => {
            let escrutinee = Box::new(elaborate_term(*scrutinee));
            let ecases = cases.into_iter().map(elaborate_case).collect();
            core::Term::Match {
                span: span,
                scrutinee: escrutinee,
                cases: ecases
            }
        }
        ast::Term::App { fun, arg, span } => {
            let efun = elaborate_term(*fun);
            let earg = elaborate_term(*arg);
            core::Term::App {
                span: span,
                fun: Box::new(efun),
                arg: Box::new(earg),
            }
        },
        ast::Term::Forall { name, ty, term, span } =>
            core::Term::Forall {
                span: span,
                name: elaborate_name(name),
                ty: Box::new(elaborate_term(*ty)),
                term: Box::new(elaborate_term(*term)),
            },
        ast::Term::Metavar { .. } => panic!("can't elaborate meta-variables"),
        ast::Term::Lambda { args, ret_ty, body, span } => {
            let eargs = args.into_iter().map(|(n, t)| {
                (elaborate_name(n), elaborate_term(t))
            }).collect();
            let eret_ty = elaborate_term(*ret_ty);
            let ebody = elaborate_term(*body);

            core::Term::Lambda {
                span: span,
                args: eargs,
                ret_ty: Box::new(eret_ty),
                body: Box::new(ebody),
            }
        }
        ast::Term::Type => core::Term::Type,
    }
}

fn elaborate_literal(lit: ast::Literal) -> core::Literal {
    match lit {
        ast::Literal::Unit => core::Literal::Unit,
        ast::Literal::Int(i) => core::Literal::Int(i),
    }
}

fn elaborate_name(n: ast::Name) -> core::Name {
    core::Name {
        repr: n.repr,
        span: n.span,
    }
}

fn elaborate_case(case: ast::Case) -> core::Case {
    match case {
        ast::Case { pattern, rhs, .. } => core::Case {
            pattern: elaborate_pattern(pattern),
            rhs: elaborate_term(rhs),
        }
    }
}

fn elaborate_pattern(pattern: ast::Pattern) -> core::Pattern {
    match pattern {
        ast::Pattern::Name(n) =>
            core::Pattern::Simple(
                core::SimplePattern::Name(elaborate_name(n))),
        ast::Pattern::Constructor(n, patterns) =>
            core::Pattern::Constructor(
                elaborate_name(n),
                patterns.into_iter()
                        .map(elaborate_simple_pattern)
                        .collect()),
        _ => panic!("elaboration error")
    }
}

fn elaborate_simple_pattern(pattern: ast::Pattern) -> core::SimplePattern {
    match pattern {
        ast::Pattern::Name(n) => core::SimplePattern::Name(elaborate_name(n)),
        _ => panic!("elaboration error")
    }
}
