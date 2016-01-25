extern crate lalrpop_util;
extern crate llvm_sys;
extern crate gcc;
#[macro_use] extern crate log;
extern crate iron;
extern crate hubris_parser;
extern crate term;

pub mod ast {
    pub use hubris_parser::ast::*;
}

// pub mod backend;
pub mod core;
// pub mod cps;
pub mod elaborate;
pub mod llvm;

pub mod parser {
    pub use hubris_parser::parser::*;
}

pub mod server;
pub mod typeck;

use typeck::*;

use std::path::{PathBuf, Path};
use std::io;

pub fn compile_file<T: AsRef<Path>>(path: T, _output: Option<PathBuf>) -> io::Result<()> {
    let parser = try!(parser::from_file(path.as_ref()));
    let module = parser.parse();
    let emodule = elaborate::elaborate_module(
                        path.as_ref(),
                        module,
                        parser.source_map.clone());

    let emodule = match emodule {
        Err(e) => panic!("elaboration error: {:?}", e),
        Ok(v) => v,
    };

    let ty_cx = TyCtxt::from_module(&emodule, parser.source_map);

    let term = term::stdout().unwrap();

    for def in &emodule.defs {
        match ty_cx.type_check_def(def) {
            Err(err) => {
                report_type_error(&ty_cx, term, err).unwrap(); // handle this properly 
                return Ok(());
            }
            Ok(_) => {
                let t = ty_cx.get_main_body();
                println!("main={}",
                    ty_cx.eval(t).unwrap());
            }
        }
    }

    // let cps_module = cps::from_core_module(emodule);
    // println!("{:?}", cps_module);

    //backend::create_executable(&cps_module, output);
    Ok(())
}
