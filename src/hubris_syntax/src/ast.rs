use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};
use std::path::PathBuf;

use pretty::*;

pub use parser::SourceMap;

pub trait HasSpan {
    fn get_span(&self) -> Span;
    fn set_span(&mut self, span: Span);
}

#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct ModuleId(pub usize);
/// A Span the start and end of a subset of a string. It can span multiple lines
#[derive(Copy, Clone, Debug, Hash, PartialEq, Eq, PartialOrd, Ord)]
pub struct Span {
    pub module_id: ModuleId,
    pub lo: usize,
    pub hi: usize,
}

impl Span {
    #[inline]
    pub fn new(lo: usize, hi: usize) -> Span {
        Span {
            module_id: ModuleId(0),
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

impl Pretty for Name {
    fn pretty(&self) -> Doc {
        use self::NameKind::*;

        let s = match &self.repr {
            &Unqualified(ref s) => s.clone(),
            &Qualified(ref qn) => format!("{:?}", qn),
            &Placeholder => String::from("_"),
        };
        Doc::text(s)
    }
}

impl Display for Name {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        format(self, formatter)
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
    pub id: ModuleId,
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
            id: ModuleId(0),
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
    Axiom(Axiom),
    Extern(Extern),
    Comment(String),
    Import(Name),
}

impl HasSpan for Item {
    fn get_span(&self) -> Span {
        use self::Item::*;

        match self {
            &Inductive(ref data) => data.span,
            &Def(ref fun) => fun.span,
            &Axiom(ref a) => a.span,
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
            &mut Axiom(ref mut a) =>
                a.span = sp,
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
    pub names: Vec<Name>,
    pub ty: Option<Term>,
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
pub struct Axiom {
    pub span: Span,
    pub name: Name,
    pub ty: Term,
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

impl Display for Term {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        format(self, formatter)
    }
}

impl Pretty for Term {
    fn pretty(&self) -> Doc {
        use self::Term::*;

        match self {
            &Var { ref name, .. } => name.pretty(),
            &App { ref fun, ref arg, .. } => {
                fun.pretty() + " ".pretty() + parens(arg.pretty())
            }
            &Forall { ref binders, ref term, .. } => {
                panic!()
                // if binder.name.is_placeholder() {
                //     let p = match &*binder.ty {
                //         &Forall {..} => parens(binder.ty.pretty()) + " -> ".pretty(),
                //         _ => binder.ty.pretty() + " -> ".pretty(),
                //     };
                //     p + term.pretty()
                // } else {
                //     let mut cursor = &**term;
                //     let mut binders = Vec::new();
                //     binders.push(binder);
                //     while let &Term::Forall { ref binder, ref term, .. } = cursor {
                //         // This is because we only want to pretty print the chunk of
                //         // binders up to a placeholder name.
                //         if binder.name.is_placeholder() { break; }
                //         binders.push(binder);
                //         cursor = term;
                //     }
                //     "forall ".pretty() + pretty_binders(binders.as_slice()) +
                //         ", ".pretty() + cursor.pretty()
                // }
            }
            &Lambda { ref args, ref body, .. } => {
                panic!()
                // // Now we pretty print the function with the collesced binders.
                // let mut cursor = &**body;
                // let mut binders = Vec::new();
                // binders.push(binder);
                // while let &Term::Lambda { ref binder, ref body, .. } = cursor {
                //     // This is because we only want to pretty print the chunk of
                //     // binders up to a placeholder name.
                //     if binder.name.is_placeholder() { break; }
                //     binders.push(binder);
                //     cursor = body;
                // }
                //
                // "fun ".pretty() + pretty_binders(binders.as_slice()) + " => ".pretty() + cursor.pretty()
            }
            &Let { .. } => panic!(),
            &Match { ref scrutinee, ref cases, .. } => {
                let cases : Vec<_> = cases.iter().map(|x| x.pretty()).collect();
                "match ".pretty() + scrutinee.pretty() + " with\n".pretty() +
                seperate(&cases[..], &"\n".pretty()) + "\nend".pretty()
            }
            &Literal { .. } => panic!(),
            &Type => Doc::text("Type"),
        }
    }
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

impl Display for Case {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        format(self, formatter)
    }
}

impl Pretty for Case {
    fn pretty(&self) -> Doc {
        use self::Term::*;
        "| ".pretty() + self.pattern.pretty() + " => ".pretty() + self.rhs.pretty()
    }
}

#[derive(Debug, PartialEq, Clone)]
pub enum Pattern {
    Name(Name),
    Constructor(Name, Vec<Pattern>),
    Placeholder,
}

impl Display for Pattern {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        format(self, formatter)
    }
}

impl Pretty for Pattern {
    fn pretty(&self) -> Doc {
        use self::Pattern::*;
        match self {
            &Name(ref n) => n.pretty(),
            &Constructor(ref n, ref pats) => {
                let pats: Vec<_> = pats.iter().map(|p| parens(p.pretty())).collect();
                n.pretty() + seperate(&pats[..], &" ".pretty())
            }
            &Placeholder => "_".pretty(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Literal {
    Int(i64), // will need to revisit this decision
    Unit
}
