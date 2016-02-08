use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::path::PathBuf;

pub use parser::SourceMap;

pub trait HasSpan {
    fn get_span(&self) -> Span;
    fn set_span(&mut self, span: Span);
}

/// A Span the start and end of a subset of a string. It can span multiple lines
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

#[derive(Clone, Debug, Eq, PartialOrd, Ord)]
pub struct Name {
    pub span: Span,
    pub repr: NameKind,
}

#[derive(Clone, Debug, PartialEq, Eq, PartialOrd, Ord, Hash)]
pub enum NameKind {
    Unqualified(String),
    Qualified(Vec<String>),
    Placeholder,
}

impl Name {
    pub fn from_str(s: &str) -> Name {
        Name {
            span: Span::dummy(),
            repr: NameKind::Unqualified(s.to_owned()),
        }
    }

    pub fn qualified(components: Vec<String>) -> Name {
        Name {
            span: Span::dummy(),
            repr: NameKind::Qualified(components),
        }
    }

    pub fn in_scope(&self, component: String) -> Option<Name> {
        match &self.repr {
            &NameKind::Qualified(ref components) => {
                let mut components = components.clone();
                components.push(component);

                Some(Name {
                    span: self.span,
                    repr: NameKind::Qualified(components),
                })
            }
            &NameKind::Unqualified(ref n) => {
                Some(Name {
                    span: self.span,
                    repr: NameKind::Qualified(vec![n.clone(), component]),
                })
            }
            _ => None,
        }
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Name) -> bool {
        self.repr == other.repr
    }
}

impl Hash for Name {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        self.repr.hash(state)
    }
}

impl Display for Name {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        use self::NameKind::*;

        match &self.repr {
            &Unqualified(ref s) => write!(formatter, "{}", s),
            &Qualified(ref qn) => write!(formatter, "{:?}", qn),
            &Placeholder => write!(formatter, "_"),
        }
    }
}

impl HasSpan for Name {
    fn get_span(&self) -> Span {
        self.span
    }

    fn set_span(&mut self, sp: Span) {
        self.span = sp
    }
}

#[derive(Debug)]
pub struct Module {
    pub span: Span,
    pub name: Name,
    pub items: Vec<Item>,
}

impl Module {
    pub fn file_name(&self) -> PathBuf {
        PathBuf::from(&self.name.to_string()[..])
    }

    pub fn empty() -> Module {
        Module {
            span: Span::dummy(),
            name: Name::from_str("REPL"),
            items: vec![],
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Item {
    Inductive(Inductive),
    Def(Def),
    Extern(Extern),
    Comment(()),
    Import(Name),
}

impl HasSpan for Item {
    fn get_span(&self) -> Span {
        use self::Item::*;

        match self {
            &Inductive(ref data) => data.span,
            &Def(ref fun) => fun.span,
            &Extern(ref ext) => ext.span,
            &Comment(_) => Span::dummy(),
            &Import(_) => Span::dummy(),
        }
    }

    fn set_span(&mut self, sp: Span) {
        use self::Item::*;

        match self {
            &mut Inductive(ref mut inductive) =>
                inductive.span = sp,
            &mut Def(ref mut def) =>
                def.span = sp,
            &mut Extern(ref mut ext) =>
                ext.span = sp,
            &mut Comment(_) => {},
            &mut Import(_) => {},
        }
    }
}

#[derive(PartialEq, Debug, Clone)]
pub enum BindingMode {
    Explicit,
    Implicit,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Binder {
    pub span: Span,
    pub name: Name,
    pub ty: Term,
    pub mode: BindingMode,
}

#[derive(PartialEq, Debug, Clone)]
pub struct Inductive {
    pub span: Span,
    pub name: Name,
    pub parameters: Vec<Binder>,
    pub ty: Term,
    pub ctors: Vec<Constructor>
}

pub type Constructor = (Name, Term);

#[derive(Debug, PartialEq, Clone)]
pub struct Extern {
    pub span: Span,
    pub name: Name,
    pub term: Term,
}

#[derive(Debug, PartialEq, Clone)]
pub struct Def {
    pub span: Span,
    pub name: Name,
    pub args: Vec<Binder>,
    pub ty: Term,
    pub body: Term,
}

#[derive(Debug, PartialEq, Clone)]
pub enum Term {
    Literal { span: Span, lit: Literal },
    Var { name: Name },
    Match { span: Span, scrutinee: Box<Term>, cases: Vec<Case> },
    App { span: Span, fun: Box<Term>, arg: Box<Term> },
    Forall { span: Span, binders: Vec<Binder>, term: Box<Term> },
    Lambda { span: Span, args: Vec<Binder>, ret_ty: Box<Option<Term>>, body: Box<Term> },
    Let { span: Span, bindings: Vec<(Binder, Term)>, body: Box<Term> },
    Type,
}

impl HasSpan for Term {
    fn get_span(&self) -> Span {
        use self::Term::*;

        match self {
            &Literal { span, .. } => span,
            &Var { ref name } => name.span,
            &Match { span, .. } => span,
            &App { span, .. } => span,
            &Forall { span, .. } => span,
            &Lambda { span, .. } => span,
            &Let { span, .. } => span,
            &Type => Span::dummy(),
        }
    }

    fn set_span(&mut self, sp: Span) {
        use self::Term::*;

        match self {
            &mut Literal { ref mut span, .. } => *span = sp,
            &mut Var { ref mut name } => name.span = sp,
            &mut Match { ref mut span, .. } => *span = sp,
            &mut App { ref mut span, .. } => *span = sp,
            &mut Forall { ref mut span, .. } => *span = sp,
            &mut Lambda { ref mut span, .. } => *span = sp,
            &mut Let { ref mut span, .. } => *span = sp,
            &mut Type => {},
        }
    }
}

#[derive(Debug, PartialEq, Clone)]
pub struct Case {
    pub span: Span,
    pub pattern: Pattern,
    pub rhs: Term,
}

#[derive(Debug, PartialEq, Clone)]
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
