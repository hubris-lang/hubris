use std::fmt::Debug;
use std::path::{Path};
use super::core;
// #[derive(Debug, Clone)]
// enum Error {
//     UnknownSymbol(String),
// }

/// A trait that describes the interface to a particular compiler backend.
pub trait Backend {
    fn create_executable<P: AsRef<Path> + Debug>(module: core::Module, output: Option<P>);
}

pub struct Rust;

struct Module {
    //constructor: Vec<()>,
    definitions: Vec<Definition>,
}

struct Definition {
    name: core::Name,
    body: Term,
}

enum Term {
    DummyTerm
}

fn lower_module(module: core::Module) -> Module {
    Module {
        definitions:
            module.defs
                  .into_iter()
                  .filter_map(|i| match i {
                      core::Item::Fn(d) => Some(lower_def(d)),
                      _ => None,
                  })
                  .collect()
    }
}

fn lower_def(def: core::Def) -> Definition {
    panic!("{}", def);
}


impl Backend for Rust {
    fn create_executable<P: AsRef<Path> + Debug>(module: core::Module, output: Option<P>) {
        let m = lower_module(module);
        panic!();
    }
}
