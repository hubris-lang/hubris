use std::fmt::{self, Display, Formatter};
use std::path::{Path, PathBuf};
use std::hash::Hasher;

use super::ast::Span;

pub mod name;
pub mod term;

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
    pub parameters: Vec<Name>,
    pub ty: Term,
    pub ctors: Vec<Constructor>,
}

pub type Constructor = (Name, Term);


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

impl Display for Function {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        let &Function {
            ref name,
            ref ret_ty,
            ref body,
            ..
        } = self;

        try!(write!(formatter, "fn {}(", name));
        try!(write!(formatter, "{} : {}) :=", name, ret_ty));
        try!(writeln!(formatter, "{}", body));
        writeln!(formatter, "end")
    }
}
