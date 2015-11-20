#![feature(convert)]
extern crate lalrpop_util;
extern crate llvm_sys;
extern crate gcc;
#[macro_use] extern crate log;

pub mod ast;
pub mod backend;
pub mod parser;
pub mod llvm;

use std::path::Path;
use std::collections::HashMap;
use std::process::Command;

use ast::{Definition, Name, Schema, Function, Type};

/// A global context for type checking containing the necessary information
/// needed across type checking all definitions.
pub struct TyCtxt {
    types: HashMap<Name, Schema>,
    functions: HashMap<Name, Function>
}

impl TyCtxt {
    pub fn empty() -> TyCtxt {
        TyCtxt {
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }
}

/// A type checking context representing in-scope names and their types.
struct Context {
    map: HashMap<Name, Type>
}

fn type_check_def(_ty_cx: &TyCtxt, def: &ast::Definition) -> Result<(), ()> {
    match def {
        &Definition::Fn(_) => Ok(()),
        _ => Ok(())
    }
}

pub fn compile_file<T: AsRef<Path>>(path: T) {
    let module = parser::from_file(path).unwrap();
    let ty_cx = TyCtxt::empty();
    for def in &module.defs {
        type_check_def(&ty_cx, def);
    }
    println!("{:?}", module);
    backend::emit_module(module, "/Users/jroesch/Git/hubris/main.ll");

    Command::new("as")
           .arg("main.s")
           .arg("-o")
           .arg("main.o")
           .output()
           .unwrap_or_else(|_| panic!("as failed"));

    Command::new("gcc")
           .arg("rt.c")
           .arg("main.o")
           .arg("-o")
           .arg("prog")
           .output()
           .unwrap_or_else(|_| panic!("gcc failed"));
}
