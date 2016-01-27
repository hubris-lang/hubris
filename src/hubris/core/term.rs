use hubris_parser::ast::{Span, HasSpan};

use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};

use super::Name;

#[derive(Debug, Clone, Eq)]
pub enum Term {
    Literal { span: Span, lit: Literal },
    Var { name: Name },
    App { span: Span, fun: Box<Term>, arg: Box<Term> },
    Forall { span: Span, name: Name, ty: Box<Term>, term: Box<Term> },
    Lambda { span: Span, name: Name, ty: Box<Term>, body: Box<Term> },
    Recursor(Name, usize, Vec<Term>),
    Type,
}

impl Term {
    pub fn abstract_lambda(locals: Vec<Name>, t: Term) -> Term {
        let mut result = t;
        for local in locals.into_iter().rev() {
            let body = result.abstr(&local);

            let (repr, ty) = match local {
                Name::Local { repr, ty, .. } => (repr, ty),
                _ => panic!(),
            };

            result = Term::Lambda {
                name: Name::DeBruijn {
                    index: 0,
                    repr: repr,
                    span: Span::dummy(),
                },
                ty: ty,
                body: Box::new(body),
                span: Span::dummy(),
            };
        }
        result
    }

    pub fn abstract_pi(locals: Vec<Name>, t: Term) -> Term {
        let mut result = t;
        for local in locals.into_iter().rev() {
            let body = result.abstr(&local);

            let (repr, ty) = match local {
                Name::Local { repr, ty, .. } => (repr, ty),
                _ => panic!(),
            };

            result = Term::Forall {
                name: Name::DeBruijn {
                    index: 1,
                    repr: repr,
                    span: Span::dummy(),
                },
                ty: ty,
                term: Box::new(body),
                span: Span::dummy(),
            };
        }
        result
    }

    /// Abstracts a term, binding names for term.
    pub fn abstr(&self, name: &Name) -> Term {
        debug!("before Term::abstr: name={} in self={}", name, self);
        let result = self.abst(0, name);
        debug!("after Term::abstr: result={}", result);
        result
    }

    fn abst(&self, index: usize, x: &Name) -> Term {
        use self::Term::*;
        use super::Name::*;

        // debug!("subst: {} with {}", index, replacement);

        match self {
            &Literal { .. } => self.clone(),
            &Var { name: ref vname } => match vname {
                &Local { number: number1, ref repr, .. } => match x {
                    &Local { number, .. } if number == number1 => DeBruijn {
                        index: index,
                        span: Span::dummy(),
                        repr: repr.clone(),
                    }.to_term(),
                    _ => self.clone(),
                },
                &Qual { .. } | &DeBruijn { .. } | &Meta { .. } =>
                    self.clone(),
            },
            &App { ref fun, ref arg, span } =>
                App {
                    fun: Box::new(fun.abst(index, x)),
                    arg: Box::new(arg.abst(index, x)),
                    span: span,
                },
            &Forall { ref name, ref ty, ref term, span } =>
                Forall {
                    name: name.clone(),
                    ty: Box::new(ty.abst(index, x)),
                    term: Box::new(term.abst(index + 1, x)),
                    span: span,
                },
            &Lambda {  ref name, ref ty, ref body, span } =>
                Lambda {
                    name: name.clone(),
                    ty: Box::new(ty.abst(index, x)),
                    body: Box::new(body.abst(index + 1, x)),
                    span: span,
                },
            &Recursor(ref name, offset, ref ts) =>
                Recursor(name.clone(), offset,
                    ts.iter().map(|t| t.abst(index, x)).collect()),
            &Type => Type,
        }
    }

    pub fn instantiate(&self, subst: &Term) -> Term {
        debug!("instantaite: self={} subst={}", self, subst);
        assert!(subst.is_closed());
        self.replace(0, subst)
    }

    pub fn replace(&self, index: usize, subst: &Term) -> Term {
        use self::Term::*;
        use super::Name::*;

        debug!("replace: {} with {}", index, subst);

        match self {
            &Literal { .. } => self.clone(),
            &Var { ref name } => {
                match name {
                    &DeBruijn { index: i, .. } => {
                        if i == index {
                            subst.clone()
                        } else {
                            Var { name: name.clone() }
                        }
                    },
                    &Qual { .. } | &Local { .. } | &Meta { .. } =>
                        self.clone(),
                }
            }
            &App { ref fun, ref arg, span } =>
                App {
                    fun: Box::new(fun.replace(index, subst)),
                    arg: Box::new(arg.replace(index, subst)),
                    span: span,
                },
            &Forall { ref name, ref ty, ref term, span } =>
                Forall {
                    name: name.clone(),
                    ty: Box::new(ty.replace(index, subst)),
                    term: Box::new(term.replace(index + 1, subst)),
                    span: span,
                },
            &Lambda { ref name, ref ty, ref body, span } =>
                Lambda {
                    name: name.clone(),
                    ty: Box::new(ty.replace(index, subst)),
                    body: Box::new(body.replace(index + 1, subst)),
                    span: span,
                },
            &Recursor(ref name, offset, ref ts) =>
                Recursor(name.clone(), offset, ts.iter().map(|t| t.replace(index, subst)).collect()),
            &Type => Type,
        }
    }

    pub fn is_closed(&self) -> bool {
        use self::Term::*;
        use super::Name::*;

        match self {
            &Var { ref name } => match name {
                &DeBruijn { .. } | &Qual { .. } | &Meta { .. } => true,
                &Local { .. } => true,
            },
            &App { ref fun, ref arg, .. } =>
                fun.is_closed() && arg.is_closed(),
            &Forall { ref ty, ref term, .. } =>
                ty.is_closed() && term.is_closed(),
            &Lambda { ref ty, ref body,.. } =>
                ty.is_closed() && body.is_closed(),
            &Recursor(..) =>
                true,
            &Literal { .. } | &Type => true
        }
    }

    pub fn apply(t: Term, u: Term) -> Term {
        Term::App {
            fun: Box::new(t),
            arg: Box::new(u),
            span: Span::dummy(),
        }
    }

    pub fn apply_all(fun : Term, args: Vec<Term>) -> Term {
        let mut result = fun;
        for arg in args {
            result = Term::App {
                fun: Box::new(result),
                arg: Box::new(arg),
                span: Span::dummy(),
            };
        }
        result
    }

    pub fn whnf(&self) -> Term {
        use self::Term::*;

        match self {
            &App { ref fun, ref arg, .. } => match &**fun {
                &Term::Forall { ref term, .. } |
                &Term::Lambda { body: ref term, .. } =>
                    term.instantiate(arg).whnf(),
                _ => self.clone(),
            },
            &Forall { ref name, ref ty, ref term, span } => {
                Term::Forall {
                    name: name.clone(),
                    ty: Box::new(ty.whnf()),
                    term: Box::new(term.whnf()),
                    span: span,
                }
            }
            &Lambda { ref name, ref ty, ref body, span } => {
                Term::Lambda {
                    name: name.clone(),
                    ty: Box::new(ty.whnf()),
                    body: Box::new(body.whnf()),
                    span: span,
                }
            }
            &Recursor(..) =>
                panic!(),
            t => t.clone(),
        }
    }

    /// Will return the "head" of a term
    pub fn head(&self) -> Option<Term> {
        use self::Term::*;
        match self {
            &App { ref fun, .. } => {
                let mut result : &Term = &**fun;
                while let &App { ref fun, .. } = result {
                    result = &**fun;
                }
                Some(result.clone())
            },
            f @ &Forall { .. } => Some(f.clone()),
            l @ &Lambda { .. } => Some(l.clone()),
            v @ &Var { .. } => Some(v.clone()),
            &Recursor(..) =>
                panic!(),
            _ => None,
        }
    }

    pub fn args(&self) -> Option<Vec<Term>> {
        use self::Term::*;
        match self {
            &App { ref fun, ref arg, ..} => {
                let mut f = &**fun;
                let mut result = vec![*arg.clone()];
                while let &App { ref fun, ref arg, .. } = f {
                    f = &**fun;
                    result.push(*arg.clone());
                }
                Some(result.into_iter().rev().collect())
            },
            _ => None,
        }
    }

    // Replace all sub-terms satisfying pred.
    pub fn replace_term<F: Fn(&Term) -> bool>(&mut self, replacement: &Term, pred: &F) {
        use self::Term::*;

        if pred(self) {
            *self = replacement.clone();
        } else {
            match self {
                &mut App { ref mut fun, ref mut arg, .. } => {
                    fun.replace_term(&replacement, pred);
                    arg.replace_term(&replacement, pred);
                }
                &mut Forall { ref mut ty, ref mut term, .. } => {
                    ty.replace_term(&replacement, pred);
                    term.replace_term(&replacement, pred);
                }
                &mut Lambda { ref mut ty, ref mut body, .. } => {
                    ty.replace_term(&replacement, pred);
                    body.replace_term(&replacement, pred);
                }
                &mut Recursor(..) => {
                    panic!()
                }
                _ => {}
            }
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
            (&App { fun: ref fun1, arg: ref arg1, .. },
             &App { fun: ref fun2, arg: ref arg2, .. }) =>
                fun1 == fun2 && arg1 == arg2,
            (&Forall { name: ref name1, ty: ref ty1, term: ref term1, .. },
             &Forall { name: ref name2, ty: ref ty2, term: ref term2, .. }) =>
                name1 == name2 && ty1 == ty2 && term1 == term2,
            (&Lambda { name: ref args1, ty: ref ret_ty1, body: ref body1, .. },
             &Lambda { name: ref args2, ty: ref ret_ty2, body: ref body2, ..}) =>
                args1 == args2 && ret_ty1 == ret_ty2 && body1 == body2,
            // TODO: Deal with Intro/Recursor eq
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
            &Literal { ref lit, .. } => {
                0.hash(state);
                lit.hash(state)
            },
            &Var { ref name, .. } => {
                1.hash(state);
                name.hash(state);
            },
            &App { ref fun, ref arg, .. } => {
                2.hash(state);
                fun.hash(state);
                arg.hash(state);
            },
            &Forall { ref name, ref ty, ref term, .. } => {
                3.hash(state);
                name.hash(state);
                ty.hash(state);
                term.hash(state);
            },
            &Lambda { ref name, ref ty, ref body, .. } => {
                4.hash(state);
                name.hash(state);
                ty.hash(state);
                body.hash(state);
            },
            &Recursor(..) => {
                5.hash(state);
            },
            &Type => {
                6.hash(state);
            }
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
            &App { ref fun, ref arg, .. } =>
                match &**arg {
                    &Term::App { .. } => write!(formatter, "{} ({})", fun, arg),
                    _ => write!(formatter, "{} {}", fun, arg),
                },
            &Forall { ref name, ref ty, ref term, .. } =>
                // TODO implement PartialEq on strings or create special placeholder name
                if name.is_placeholder() {
                    match &**ty {
                        &Forall {..} => try!(write!(formatter, "({}) -> ", ty)),
                        _ => try!(write!(formatter, "{} -> ", ty)),
                    }
                    write!(formatter, "{}", term)
                } else {
                    write!(formatter, "forall ({} : {}), {}", name, ty, term)
                },
            &Lambda { ref name, ref ty, ref body, .. } => {
                write!(formatter, "fun ({} : {}) => {}", name, ty, body)
            }
            &Recursor(ref name, _, ref ts) => {
                try!(writeln!(formatter, "recursor({}): {{", name));
                for t in ts {
                    try!(writeln!(formatter, "{}", t));
                }
                try!(writeln!(formatter, "}}"));
                Ok(())
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
            &App { span, .. } => span,
            &Forall { span, .. } => span,
            &Lambda { span, .. } => span,
            &Type => Span::dummy(),
            _ => panic!(),
        }
    }

    fn set_span(&mut self, sp: Span) {
        use self::Term::*;

        match self {
            &mut Literal { ref mut span, .. } => *span = sp,
            &mut Var { ref mut name } => name.set_span(sp),
            &mut App { ref mut span, .. } => *span = sp,
            &mut Forall { ref mut span, .. } => *span = sp,
            &mut Lambda { ref mut span, .. } => *span = sp,
            &mut Type => {},
            _ => panic!(),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Literal {
    Int(i64), // will need to revisit this decision
    Unit
}
