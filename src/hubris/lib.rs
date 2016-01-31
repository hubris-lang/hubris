#![feature(custom_derive, plugin)]
#![plugin(serde_macros)]

extern crate gcc;
extern crate iron;
extern crate hubris_parser;
#[macro_use] extern crate log;
extern crate llvm_sys;
extern crate readline;
extern crate rmp_serde;
extern crate serde;
extern crate term;

pub mod ast {
    pub use hubris_parser::ast::*;
}

// pub mod backend;
pub mod core;
// pub mod cps;
pub mod error_reporting;
pub mod elaborate;
pub mod llvm;

pub mod parser {
    pub use hubris_parser::parser::*;
}

pub mod repl;
pub mod server;
pub mod typeck;

use std::path::{PathBuf, Path};
use std::io;

pub fn compile_file<T: AsRef<Path>>(path: T, _output: Option<PathBuf>) -> io::Result<()> {
    let parser = try!(parser::from_file(path.as_ref()));
    let module = parser.parse();
    let mut ecx = elaborate::ElabCx::from_module(module, parser.source_map.clone());

    let emodule = ecx.elaborate_module(path.as_ref());

    let emodule = match emodule {
        Err(e) => panic!("elaboration error: {:?}", e),
        Ok(v) => v,
    };

    let term = term::stdout().unwrap();

    let t = ecx.ty_cx.get_main_body();
    println!("main={}", ecx.ty_cx.eval(t).unwrap());

    // let cps_module = cps::from_core_module(emodule);
    // println!("{:?}", cps_module);

    //backend::create_executable(&cps_module, output);
    Ok(())
}
