extern crate lalrpop_util;
extern crate llvm_sys;

pub mod ast;
pub mod parser;
pub mod llvm;

use std::path::Path;
use std::collections::HashMap;

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
    let program = parser::from_file(path).unwrap();
    let ty_cx = TyCtxt::empty();
    for def in &program {
        type_check_def(&ty_cx, def);
    }
    println!("{:?}", program);
    llvm::generate_ir("main_module".to_string(), "./");
    llvm::tools::llc("./");
    // run llc
    // build C entry point
    // link
}
