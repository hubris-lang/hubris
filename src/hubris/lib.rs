#![feature(convert)]
extern crate lalrpop_util;
extern crate llvm_sys;
extern crate gcc;
#[macro_use] extern crate log;
extern crate iron;
extern crate hubris_parser;

pub mod ast {
    pub use hubris_parser::ast::*;
}

pub mod backend;
pub mod core;
pub mod cps;
pub mod elaborate;
pub mod llvm;

pub mod parser {
    pub use hubris_parser::parser::*;
}

pub mod server;
pub mod typeck;

use std::path::{PathBuf, Path};

use typeck::*;

pub fn compile_file<T: AsRef<Path>>(path: T, output: Option<PathBuf>) {
    let module = parser::from_file(path).unwrap();
    let emodule = elaborate::elaborate_module(module);
    let ty_cx = TyCtxt::from_module(&emodule);

    for def in &emodule.defs {
        match ty_cx.type_check_def(def) {
            Err(err) =>
                panic!("error encountered while type checking: {:?}", err),
            Ok(_) => {}
        }
    }

    let cps_module = cps::from_core_module(emodule);
    println!("{:?}", cps_module);

    backend::create_executable(&cps_module, output);
}
