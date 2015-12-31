use hubris_parser::ast::{Span, HasSpan};

use std::fmt::{self, Display, Formatter};
use std::path::PathBuf;
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug, Eq)]
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
        write!(formatter, "{}", self.repr)
    }
}

impl HasSpan for Name {
    fn get_span(&self) -> Span {
        self.span
    }

    fn set_span(&mut self, span: Span) {
        self.span = span;
    }
}

#[derive(Debug)]
pub struct Module {
    // Eventually we should use ID's that map to files
    pub file_name: PathBuf,
    pub name: Name,
    pub defs: Vec<Definition>,
}

impl Module {
    pub fn file_name(&self) -> PathBuf {
        PathBuf::from(&self.name.repr[..])
    }
}

#[derive(PartialEq, Clone, Debug)]
pub struct Data {
    pub span: Span,
    pub name: Name,
    pub ty: Term,
    pub ctors: Vec<(Name, Term)>
}

#[derive(Debug, Clone, PartialEq)]
pub struct Extern {
    pub span: Span,
    pub name: Name,
    pub term: Term
}

#[derive(Debug, Clone, PartialEq)]
pub enum Definition {
    Data(Data),
    Fn(Function),
    Extern(Extern),
}

#[derive(Debug, Clone, PartialEq)]
pub struct Function {
    pub name: Name,
    pub args: Vec<(Name, Term)>,
    pub ty: Term,
    pub body: Term,
}

impl Function {
    pub fn ty(&self) -> Term {
        let mut result = self.ty.clone();
        for &(ref n, ref t) in self.args.iter().rev() {
            result = Term::Forall {
                span: Span::dummy(),
                name: n.clone(),
                ty: Box::new(t.clone()),
                term: Box::new(result)
            };
        }
        return result;
    }
}

#[derive(Debug, Clone, Eq)]
pub enum Term {
    Literal { span: Span, lit: Literal },
    Var { name: Name },
    Match { span: Span, scrutinee: Box<Term>, cases: Vec<Case> },
    App { span: Span, fun: Box<Term>, arg: Box<Term> },
    Forall { span: Span, name: Name, ty: Box<Term>, term: Box<Term> },
    // Metavar { name: Name },
    Lambda { span: Span, args: Vec<(Name, Term)>, ret_ty: Box<Term>, body: Box<Term> },
    Type,
}

impl Term {
    pub fn subst(&self, n: &Name, replacement: &Term) -> Term {
        use self::Term::*;

        debug!("subst: {} with {}", n, replacement);

        match self {
            &Literal { .. } => self.clone(),
            &Var { ref name } =>
                if name == n {
                    replacement.clone()
                } else {
                    Var { name: name.clone() }
                },
            &Match { .. } =>
                panic!("can't subst on match"),
            &App { ref fun, ref arg, span } =>
                App {
                    fun: Box::new(fun.subst(n, replacement)),
                    arg: Box::new(arg.subst(n, replacement)),
                    span: span,
                },
            &Forall { ref name, ref ty, ref term, span } =>
                if name == n {
                    self.clone()
                } else {
                    Forall {
                        name: name.clone(),
                        ty: Box::new(ty.subst(n, replacement)),
                        term: Box::new(term.subst(n, replacement)),
                        span: span,
                    }
                },
            &Lambda { .. } =>
                panic!("can't subst on lambda"),
            &Type => Type,
        }
    }
}

impl PartialEq for Term {
    fn eq(&self, other: &Term) -> bool {
        use self::Term::*;

        match (self, other) {
            (&Literal { lit: ref lit1, .. },
             &Literal { lit: ref lit2, .. }) =>
                lit1 == lit2,
            (&Var { name: ref name1, .. },
             &Var { name: ref name2, .. }) =>
                name1 == name2,
            (&Match { .. },
             &Match {..}) => panic!(),
            (&App { fun: ref fun1, arg: ref arg1, .. },
             &App { fun: ref fun2, arg: ref arg2, .. }) =>
                fun1 == fun2 && arg1 == arg2,
            (&Forall { name: ref name1, ty: ref ty1, term: ref term1, .. },
             &Forall { name: ref name2, ty: ref ty2, term: ref term2, .. }) =>
                name1 == name2 && ty1 == ty2 && term1 == term2,
            (&Lambda { args: ref args1, ret_ty: ref ret_ty1, body: ref body1, .. },
             &Lambda { args: ref args2, ret_ty: ref ret_ty2, body: ref body2, ..}) =>
                args1 == args2 && ret_ty1 == ret_ty2 && body1 == body2,
            (&Type, &Type) => true,
            _ => false
        }
    }
}

impl Hash for Term {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        use self::Term::*;

        match self {
            &Literal { ref lit, .. } =>
                lit.hash(state),
            &Var { ref name, .. } =>
                name.hash(state),
            &Match { .. } => panic!(),
            &App { ref fun, ref arg, .. } => {
                fun.hash(state);
                arg.hash(state);
            },
            &Forall { ref name, ref ty, ref term, .. } => {
                name.hash(state);
                ty.hash(state);
                term.hash(state);
            },
            &Lambda { ref args, ref ret_ty, ref body, .. } => {
                args.hash(state);
                ret_ty.hash(state);
                body.hash(state);
            },
            &Type => Type.hash(state),
        }
    }
}

impl Display for Term {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        use self::Term::*;

        match self {
            &Literal { ref lit, .. } =>
                write!(formatter, "{:?}", lit),
            &Var { ref name, .. } =>
                write!(formatter, "{}", name),
            &Match { .. } => panic!(),
            &App { ref fun, ref arg, .. } =>
                write!(formatter, "{} {}", fun, arg),
            &Forall { ref name, ref ty, ref term, .. } =>
                if name.repr == "" {
                    write!(formatter, "{} -> {}", ty, term)
                } else {
                    write!(formatter, "forall ({} : {}), {}", name, ty, term)
                },
            &Lambda { ref args, ref ret_ty, ref body, .. } =>
                write!(formatter, "lambda"),
            &Type => write!(formatter, "Type"),
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
            &mut Type => {},
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i64), // will need to revisit this decision
    Unit
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub struct Case {
    pub pattern: Pattern,
    pub rhs: Term,
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Pattern {
    Constructor(Name, Vec<SimplePattern>),
    Simple(SimplePattern),
}

impl Pattern {
    pub fn to_term(&self) -> Term {
        match self {
            &Pattern::Constructor(ref n, ref pats) => {
                let mut term = Term::Var { name: n.clone() };
                for pat in pats.iter().rev() {
                    match pat {
                        &SimplePattern::Name(ref n) => {
                            term = Term::App {
                                fun: Box::new(term),
                                arg: Box::new(Term::Var { name: n.clone() }),
                                span: Span::dummy(),
                            };
                        }
                        _ => panic!()
                    }
                }
                return term;
            },
            &Pattern::Simple(ref simple_pattern) => {
                panic!()
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SimplePattern {
    Placeholder,
    Name(Name),
}
