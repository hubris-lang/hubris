use std::fmt::{self, Display, Formatter};
use std::path::{Path, PathBuf};
use std::hash::Hasher;

use super::ast::Span;

extern crate pretty;
use self::pretty::*;

pub mod binder;
pub mod name;
pub mod term;
// pub mod visit;
// pub mod validate;
pub use self::binder::*;
pub use self::name::*;
pub use self::term::*;

#[derive(Debug)]
pub struct Module {
    // Eventually we should use ID's that map to files
    pub file_name: PathBuf,
    pub name: Name,
    pub imports: Vec<Name>,
    pub defs: Vec<Item>,
}

impl Module {
    pub fn file_name(&self) -> &Path {
        &self.file_name
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Data {
    pub span: Span,
    pub name: Name,
    // Not sure if this is the best choice, should it be a binder?
    // we try to strip as much high level structure as possible.
    pub parameters: Vec<Name>,
    pub ty: Term,
    pub ctors: Vec<Constructor>,
}

pub type Constructor = (Name, Term);

#[derive(Debug, Clone, PartialEq)]
pub struct Axiom {
    pub span: Span,
    pub name: Name,
    pub ty: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub struct Extern {
    pub span: Span,
    pub name: Name,
    pub term: Term,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Item {
    Data(Data),
    Fn(Function),
    Axiom(Axiom),
    Extern(Extern),
}

impl Display for Item {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        use self::Item::*;

        match self {
            &Fn(ref fun) => write!(formatter, "{}", fun),
            d => write!(formatter, "{:?}", d),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Name,
    pub args: Vec<Name>,
    pub ret_ty: Term,
    pub body: Term,
}

impl Pretty for Function {
    fn pretty(&self) -> Doc {
        let &Function {
            ref name,
            ref ret_ty,
            ref body,
            ..
        } = self;
        Pretty::pretty("fn ") + Pretty::pretty(name) + parens(Pretty::pretty(name) + Pretty::pretty(" :") + Pretty::pretty(ret_ty)) + Pretty::pretty(":=") + Pretty::pretty(body) + Pretty::pretty("end")
    }
}

impl Display for Function {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        format(self, formatter)
    }
}
