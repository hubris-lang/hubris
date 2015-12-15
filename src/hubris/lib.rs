#![feature(convert)]
extern crate lalrpop_util;
extern crate llvm_sys;
extern crate gcc;
#[macro_use] extern crate log;

pub mod ast;
pub mod backend;
pub mod core;
pub mod cps;
pub mod elaborate;
pub mod llvm;
pub mod parser;
pub mod server;
pub mod typeck;


use std::env;
use std::path::{PathBuf, Path};
use std::collections::HashMap;
use std::process::Command;

use ast::{Name, Term};
use typeck::*;

/// A type checking context representing in-scope names and their types.
struct Context {
    map: HashMap<Name, Term>
}

pub fn compile_file<T: AsRef<Path>>(path: T, output: Option<PathBuf>) {
    let module = parser::from_file(path).unwrap();
    let ty_cx = TyCtxt::empty();

    let emodule = elaborate::elaborate_module(module);

    for def in &emodule.defs {
        match ty_cx.type_check_def(def) {
            Err(err) => panic!("error encountered while type checking {:?}", 1),
            Ok(_) => {}
        }
    }

    let cps_module = cps::from_core_module(emodule);
    println!("{:?}", cps_module);

    backend::create_executable(&cps_module, output);
}
