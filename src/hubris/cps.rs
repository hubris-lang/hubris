use core;
use core::Name;

use std::path::PathBuf;

#[derive(Debug)]
pub struct Module {
    pub name: Name,
    pub defs: Vec<Definition>,
}

impl Module {
    pub fn file_name(&self) -> PathBuf {
        PathBuf::from(&self.name[..])
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Value {
    Var(Name),
    Literal(core::Literal),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    Data(core::Data),
    Fn(Function),
    Extern(core::Extern),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Name,
    pub args: Vec<Name>,
    pub body: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Term {
    Value(Value),
    App(Value, Value),
    Fix(Vec<(Name, Vec<Name>, Box<Term>)>, Box<Term>),
}

pub fn from_core_module(core_module: core::Module) -> Module {
    let mut module = Module {
        name: core_module.name,
        defs: vec![],
    };

    for def in core_module.defs {
        match def {
            core::Definition::Data(data) =>
                module.defs.push(Definition::Data(data)),
            core::Definition::Fn(fun) =>
                module.defs.push(Definition::Fn(from_core_fn(fun))),
            core::Definition::Extern(ext) =>
                module.defs.push(Definition::Extern(ext)),
        }
    }

    return module;
}

fn from_core_fn(core_fn: core::Function) -> Function {
    Function {
        name: core_fn.name,
        args: core_fn.args.into_iter().map(|x| x.0).collect(),
        body: from_core_term(core_fn.body),
    }
}

fn from_core_term(core_term: core::Term) -> Term {
    match core_term {
        core::Term::Var(n) => Term::Value(Value::Var(n)),
        core::Term::Literal(l) => Term::Value(Value::Literal(l)),
        core::Term::App(f, g) => {
            bind_terms(vec![*f, *g], |names| {
                Term::App(Value::Var(names[0].clone()), Value::Var(names[1].clone()))
            })
        }

        f => panic!("unsupported form: {:?}", f)
    }
}

fn bind_terms<F: FnOnce(Vec<Name>) -> Term>(ts: Vec<core::Term>, body: F) -> Term {
    let names = (0..ts.len()).map(|i| format!("arg{}", i)).collect();

    let bindings = ts.into_iter()
                     .enumerate()
                     .map(|(i, t)| (format!("arg{}", i), vec![], Box::new(from_core_term(t))))
                     .collect();

    Term::Fix(bindings, Box::new(body(names)))
}
