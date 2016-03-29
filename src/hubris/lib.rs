#[macro_use]
extern crate cfg_if;
extern crate gcc;
extern crate iron;
extern crate hubris_syntax;
#[macro_use]
extern crate log;
#[cfg(feature = "llvm-backend")]
extern crate llvm_sys;
extern crate readline;
extern crate router;
extern crate term;
extern crate urlencoded;
extern crate pretty;
#[macro_use]
extern crate itertools;

pub mod ast {
    pub use hubris_syntax::ast::*;
}

pub mod backend;
pub mod core;
pub mod elaborate;

#[cfg(feature = "llvm-backend")]
pub mod llvm;

pub mod parser {
    pub use hubris_syntax::parser::*;
}

pub mod repl;
pub mod server;
pub mod session;
pub mod typeck;
pub mod syntax;
pub mod util;

use std::path::{PathBuf, Path};
use std::io;

use self::session::{HasSession, Reportable};
use self::backend::{Backend, Rust};

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Elaborator(elaborate::Error),
    TypeCk(typeck::Error),
    Parser(parser::Error),
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

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Error {
        Error::Parser(err)
    }
}

impl Reportable for Error {
    fn report(self, session: &session::Session) -> io::Result<()> {
        use self::Error::*;

        match self {
            Io(io_err) => Err(io_err),
            Elaborator(elab_err) => session.report(elab_err),
            TypeCk(ty_cx_err) => session.report(ty_cx_err),
            Parser(parse_err) => session.report(parse_err),
        }
    }
}

pub fn compile_file<T: AsRef<Path>>(path: T, output: Option<PathBuf>) -> Result<(), Error> {
    let module_id = ast::ModuleId(0);
    let parser = try!(parser::from_file(path.as_ref(), module_id));
    let module = try!(parser.parse());

    let session =
        session::Session::from_root(
            path.as_ref());

    session.add_source_map_for(
        module_id,
        parser.source_map);

    let mut ecx =
        elaborate::ElabCx::from_module(
            module,
            session);

    let core_module = ecx.elaborate_module();

    match core_module {
        Err(e) => { try!(ecx.report(e)); },
        Ok(core_module) => {
            {
                let main = try!(ecx.ty_cx.get_main_body());
                let result = try!(ecx.ty_cx.eval(main));
                println!("main={}", result);
            }

            Rust::create_executable(core_module, output);
       }
   }

   Ok(())
}
