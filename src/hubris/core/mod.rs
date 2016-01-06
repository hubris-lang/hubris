use hubris_parser::ast::{Span, HasSpan};

use std::fmt::{self, Display, Formatter};
use std::path::{Path, PathBuf};
use std::hash::{Hash, Hasher};

#[derive(Clone, Debug, Eq)]
pub enum Name {
    DeBruijn { index: usize, span: Span, repr: String },
    Qual { span: Span, components: Vec<String> },
}

impl Name {
    pub fn from_str(s: &str) -> Name {
        Name::Qual {
            span: Span::dummy(),
            components: vec![s.to_owned()],
        }
    }

    pub fn to_term(&self) -> Term {
        Term::Var { name: self.clone() }
    }

    pub fn to_index(&self) -> usize {
        match self {
            &Name::DeBruijn { index, ..} => {
                index
            }
            n => panic!("subst name should never ecounter this case \
                        should type check this name={:?}", n)
        }
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Name) -> bool {
        use self::Name::*;

        match (self, other) {
            (&DeBruijn { index: ref index1, .. },
             &DeBruijn { index: ref index2, .. }) =>
                index1 == index2,
            (&Qual { components: ref components1, .. },
             &Qual { components: ref components2, .. }) =>
                components1 == components2,
            _ => false,
        }
    }
}

impl Hash for Name {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        use self::Name::*;

        match self {
            &DeBruijn { ref index, .. } => index.hash(state),
            &Qual { ref components, .. } => components.hash(state),
        }
    }
}

impl Display for Name {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        use self::Name::*;

        match self {
            &DeBruijn { ref index, .. } =>
                try!(write!(formatter, "{}", index)),
            &Qual { ref components, .. } =>
                for c in components {
                    try!(write!(formatter, "{}", c))
                }
        }

        Ok(())
    }
}

impl HasSpan for Name {
    fn get_span(&self) -> Span {
        use self::Name::*;

        match self {
            &Qual { span, .. } => span,
            &DeBruijn { span, .. } => span,
        }
    }

    fn set_span(&mut self, sp: Span) {
        use self::Name::*;

        match self {
            &mut DeBruijn { ref mut span, .. } =>
                *span = sp,
            &mut Qual { ref mut span, ..} =>
                *span = sp,
        }
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
    pub fn file_name(&self) -> &Path {
        &self.file_name
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

impl Display for Definition {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        use self::Definition::*;

        match self {
            &Fn(ref fun) => {
                write!(formatter, "{}", fun)
            },
            d => write!(formatter, "{:?}", d)
        }
    }
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

impl Display for Function {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        let &Function {
            ref name,
            ref args,
            ref ty,
            ref body
        } = self;

        try!(write!(formatter, "fn {}(", name));
        for &(ref arg, ref arg_ty) in args {
            try!(write!(formatter, "{} : {}", arg, arg_ty));
        }
        try!(writeln!(formatter, ") : {} :=", ty));
        try!(writeln!(formatter, "{}", body));
        writeln!(formatter, "end")
    }
}

#[derive(Debug, Clone, Eq)]
pub enum Term {
    Literal { span: Span, lit: Literal },
    Var { name: Name },
    Match {
        span: Span,
        scrutinee: Box<Term>,
        return_predicate: Box<Term>,
        cases: Vec<Case>
    },
    App { span: Span, fun: Box<Term>, arg: Box<Term> },
    Forall { span: Span, name: Name, ty: Box<Term>, term: Box<Term> },
    // Metavar { name: Name },
    Lambda { span: Span, args: Vec<(Name, Term)>, ret_ty: Box<Term>, body: Box<Term> },
    Type,
}

impl Term {
    pub fn subst(&self, index: usize, replacement: &Term) -> Term {
        use self::Term::*;
        use self::Name::*;

        debug!("subst: {} with {}", index, replacement);

        match self {
            &Literal { .. } => self.clone(),
            &Var { ref name } => {
                match name {
                    &DeBruijn { index: i, .. } => {
                        if i == index {
                            replacement.clone()
                        } else {
                            Var { name: name.clone() }
                        }
                    },
                    n => Var { name: n.clone() }
                }
            }
            &Match { .. } =>
                panic!("can't subst on match"),
            &App { ref fun, ref arg, span } =>
                App {
                    fun: Box::new(fun.subst(index, replacement)),
                    arg: Box::new(arg.subst(index, replacement)),
                    span: span,
                },
            &Forall { ref name, ref ty, ref term, span } =>
                Forall {
                    name: name.clone(),
                    ty: Box::new(ty.subst(index, replacement)),
                    term: Box::new(term.subst(index + 1, &replacement.clone().shift(1, 0))),
                    span: span,
                },
            &Lambda {  ref args, ref ret_ty, ref body, span } =>
                Lambda {
                    args: args.clone(),
                    ret_ty: ret_ty.clone(),
                    body: Box::new(body.subst(index + 1, &replacement.clone().shift(1, 0))),
                    span: span,
                },
            &Type => Type,
        }
    }

    fn shift(self, shift: usize, cutoff: usize) -> Term {
        use self::Term::*;
        use self::Name::*;

        debug!("shift: shift={} cutoff={}", shift, cutoff);

        match self {
            Literal { .. } => self.clone(),
            Var { ref name } => {
                let name = match name {
                    &DeBruijn { index, span, ref repr } => {
                        if index < cutoff {
                            DeBruijn { index: index, span: span, repr: repr.clone() }
                        } else {
                            DeBruijn { index: index + cutoff, span: span, repr: repr.clone() }
                        }
                    },
                    n => n.clone()
                };

                Var { name: name }
            },
            Match { .. } =>
                panic!("can't subst on match"),
            App { ref fun, ref arg, span } =>
                App {
                    fun: Box::new(fun.clone().shift(shift, cutoff)),
                    arg: Box::new(arg.clone().shift(shift, cutoff)),
                    span: span,
                },
            Forall { ref name, ref ty, ref term, span } => {
                Forall {
                    name: name.clone(),
                    ty: Box::new(ty.clone().shift(shift, cutoff)),
                    term: Box::new(term.clone().shift(shift, cutoff + 1)),
                    span: span,
                }
            },
            Lambda { args, ret_ty, body, span } => {
                Lambda {
                    span: span,
                    args: args.clone(),
                    ret_ty: Box::new(ret_ty.shift(shift, cutoff)),
                    body: Box::new(body.shift(shift, cutoff + args.len()))
                }
            },
            Type => Type,
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
        debug!("hash: {}", self);

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
            &Type => {},
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
            m @ &Match { .. } =>
                write!(formatter, "{:?}", m),
            &App { ref fun, ref arg, .. } =>
                write!(formatter, "{} {}", fun, arg),
            &Forall { ref name, ref ty, ref term, .. } =>
                // TODO implement PartialEq on strings or create special placeholder name
                if name.to_string() == "" {
                    write!(formatter, "{} -> {}", ty, term)
                } else {
                    write!(formatter, "forall ({} : {}), {}", name, ty, term)
                },
            &Lambda { ref args, ref ret_ty, ref body, .. } => {
                try!(write!(formatter, "fun ("));
                for &(ref name, ref ty) in args {
                    try!(write!(formatter, "{} : {}", name, ty));
                }
                try!(write!(formatter, ") : {} =>", ret_ty));
                write!(formatter, "{}", body)
            }
            &Type => write!(formatter, "Type"),
        }
    }
}

impl HasSpan for Term {
    fn get_span(&self) -> Span {
        use self::Term::*;

        match self {
            &Literal { span, .. } => span,
            &Var { ref name } => name.get_span(),
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
            &mut Var { ref mut name } => name.set_span(sp),
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
                match simple_pattern {
                    &SimplePattern::Name(ref n) => {
                        Term::Var { name: n.clone() }
                    }
                    &SimplePattern::Placeholder =>
                        panic!("cant convert placeholder")
                }
            }
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum SimplePattern {
    Placeholder,
    Name(Name),
}
