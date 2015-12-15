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
    Var(Name)
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
    App(Value, Value),
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
                module.defs.push(Definition::Fn(panic!())),
            core::Definition::Extern(ext) =>
                module.defs.push(Definition::Extern(panic!())),
        }
    }

    return module;
}

fn from_core_fn(core_fn: core::Function) -> Function {
    panic!()
}

fn from_core_term(core_term: core::Term) -> Term {
    panic!()
}
