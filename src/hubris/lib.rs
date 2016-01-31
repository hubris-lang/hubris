#![feature(custom_derive, plugin)]
#![plugin(serde_macros)]

extern crate gcc;
extern crate iron;
extern crate hubris_parser;
#[macro_use]
extern crate log;
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

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Elaborator(elaborate::Error),
    TypeCk(typeck::Error),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl From<elaborate::Error> for Error {
    fn from(err: elaborate::Error) -> Error {
        Error::Elaborator(err)
    }
}

impl From<typeck::Error> for Error {
    fn from(err: typeck::Error) -> Error {
        Error::TypeCk(err)
    }
}

pub fn compile_file<T: AsRef<Path>>(path: T, _output: Option<PathBuf>) -> Result<(), Error> {
    let parser = try!(parser::from_file(path.as_ref()));
    let module = parser.parse();
    let mut ecx = elaborate::ElabCx::from_module(module, parser.source_map.clone());

    let emodule = ecx.elaborate_module(path.as_ref());

    let emodule = match emodule {
        Err(e) => panic!("elaboration error: {:?}", e),
        Ok(v) => v,
    };

    let term = term::stdout().unwrap();

    {
        let main = try!(ecx.ty_cx.get_main_body());
        let result = try!(ecx.ty_cx.eval(main));
        println!("main={}", result);
    }

    // let cps_module = cps::from_core_module(emodule);
    // println!("{:?}", cps_module);

    // backend::create_executable(&cps_module, output);
    Ok(())
}
