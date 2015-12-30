use std::path::PathBuf;

pub use parser::SourceMap;

pub trait HasSpan {
    fn get_span(&self) -> Span;
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    #[inline]
    pub fn new(lo: usize, hi: usize) -> Span {
        Span {
            lo: lo,
            hi: hi,
        }
    }

    #[inline]
    pub fn dummy() -> Span {
        Span::new(0, 0)
    }
}

#[derive(Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Name {
    pub span: Span,
    pub repr: String,
}

impl Name {
    pub fn from_str(s: &str) -> Name {
        Name {
            span: Span::dummy(),
            repr: s.to_owned(),
        }
    }
}

#[derive(Debug)]
pub struct Module {
    pub span: Span,
    pub name: Name,
    pub defs: Vec<Definition>,
}

impl Module {
    pub fn file_name(&self) -> PathBuf {
        PathBuf::from(&self.name.repr[..])
    }
}

#[derive(PartialEq, Debug)]
pub struct Data {
    pub span: Span,
    pub name: Name,
    pub ty: Term,
    pub ctors: Vec<(Name, Term)>
}

#[derive(Debug, PartialEq)]
pub struct Extern {
    pub span: Span,
    pub name: Name,
    pub term: Term,
}

#[derive(Debug, PartialEq)]
pub enum Definition {
    Data(Data),
    Fn(Function),
    Extern(Extern),
    Comment(())
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub span: Span,
    pub name: Name,
    pub args: Vec<(Name, Term)>,
    pub ty: Term,
    pub body: Term,
}

#[derive(Debug, PartialEq)]
pub enum Term {
    Literal { span: Span, lit: Literal },
    Var { name: Name },
    Match { span: Span, scrutinee: Box<Term>, cases: Vec<Case> },
    App { span: Span, fun: Box<Term>, arg: Box<Term> },
    Forall { span: Span, name: Name, ty: Box<Term>, term: Box<Term> },
    Metavar { name: Name },
    Lambda { span: Span, args: Vec<(Name, Term)>, ret_ty: Box<Term>, body: Box<Term> },
    Type,
}

#[derive(Debug, PartialEq)]
pub struct Case {
    pub span: Span,
    pub pattern: Pattern,
    pub rhs: Term,
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Name(Name),
    Constructor(Name, Vec<Pattern>),
    Placeholder,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64), // will need to revisit this decision
    Unit
}
