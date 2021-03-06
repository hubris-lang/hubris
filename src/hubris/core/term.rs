use super::super::ast::{Span, HasSpan};

use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};

use super::Name;
use super::{Binder, BindingMode, pretty_binders};

use super::super::pretty::*;

#[derive(Debug, Clone, Eq)]
pub enum Term {
    Var {
        name: Name,
    },
    App {
        span: Span,
        fun: Box<Term>,
        arg: Box<Term>,
    },
    Forall {
        span: Span,
        binder: Binder,
        term: Box<Term>,
    },
    Lambda {
        span: Span,
        binder: Binder,
        body: Box<Term>,
    },
    Type,
}

impl Term {
    pub fn abstract_lambda(locals: Vec<Name>, t: Term) -> Term {
        let mut result = t;
        for local in locals.into_iter().rev() {
            let body = result.abstr(&local);

            let (repr, ty) = match local {
                Name::Local { repr, ty, .. } => (repr, ty),
                n => panic!("trying to abstract over {:?}", n),
            };

            result = Term::Lambda {
                binder: Binder::explicit(
                    Name::DeBruijn {
                        index: 0,
                        repr: repr,
                        span: Span::dummy(),
                    },
                    *ty),
                body: Box::new(body),
                span: Span::dummy(),
            };
        }
        result
    }

    pub fn abstract_pi(locals: Vec<Name>, t: Term) -> Term {
        Term::abstract_pi_internal(locals, t, None)
    }

    pub fn abstract_pi_implicit(locals: Vec<Name>, t: Term) -> Term {
        Term::abstract_pi_internal(locals, t, Some(BindingMode::Implicit))
    }

    fn abstract_pi_internal(
        locals: Vec<Name>,
        t: Term,
        binding_override: Option<BindingMode>) -> Term {

        let mut result = t;
        for local in locals.into_iter().rev() {
            let body = result.abstr(&local);

            let (repr, ty, mode) = match local {
                Name::Local { repr, ty, binding_info, .. } => match binding_override {
                    None => (repr, ty, binding_info),
                    Some(bi_override) => (repr, ty, bi_override),
                },
                _ => panic!("internal invariant violated: tried to abstract a name that is \
                             not a local constant"),
            };

            let name = Name::DeBruijn {
                index: 0,
                repr: repr,
                span: Span::dummy(),
            };

            let binder = Binder {
                name: name,
                ty: ty,
                mode: mode,
            };

            result = Term::Forall {
                binder: binder,
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

    pub fn abst(&self, index: usize, x: &Name) -> Term {
        use self::Term::*;
        use super::Name::*;

        // debug!("subst: {} with {}", index, replacement);

        match self {
            &Var { name: ref vname } => {
                match vname {
                    &Local { number: number1, ref repr, .. } => {
                        match x {
                            &Local { number, .. } if number == number1 => {
                                DeBruijn {
                                    index: index,
                                    span: Span::dummy(),
                                    repr: repr.clone(),
                                }
                                .to_term()
                            }
                            _ => self.clone(),
                        }
                    }
                    &Qual { .. } | &DeBruijn { .. } | &Meta { .. } => self.clone(),
                }
            }
            &App { ref fun, ref arg, span } => {
                App {
                    fun: Box::new(fun.abst(index, x)),
                    arg: Box::new(arg.abst(index, x)),
                    span: span,
                }
            }
            &Forall { ref binder, ref term, span } => {
                Forall {
                    binder: binder.abst(index, x),
                    term: Box::new(term.abst(index + 1, x)),
                    span: span,
                }
            }
            &Lambda {  ref binder, ref body, span } => {
                Lambda {
                    binder: binder.abst(index, x),
                    body: Box::new(body.abst(index + 1, x)),
                    span: span,
                }
            }
            &Type => Type,
        }
    }

    pub fn instantiate(&self, subst: &Term) -> Term {
        debug!("instantaite: self={} subst={}", self, subst);
        // assert!(subst.is_closed());
        self.replace(0, subst)
    }

    pub fn replace(&self, index: usize, subst: &Term) -> Term {
        use self::Term::*;
        use super::Name::*;

        debug!("replace: {} with {}", index, subst);

        match self {
            &Var { ref name } => {
                match name {
                    &DeBruijn { index: i, .. } => {
                        if i == index {
                            subst.clone()
                        } else {
                            Var { name: name.clone() }
                        }
                    }
                    &Qual { .. } | &Local { .. } | &Meta { .. } => self.clone(),
                }
            }
            &App { ref fun, ref arg, span } => {
                App {
                    fun: Box::new(fun.replace(index, subst)),
                    arg: Box::new(arg.replace(index, subst)),
                    span: span,
                }
            }
            &Forall { ref binder, ref term, span } => {
                Forall {
                    binder: binder.replace(index, subst),
                    term: Box::new(term.replace(index + 1, subst)),
                    span: span,
                }
            }
            &Lambda { ref binder, ref body, span } => {
                Lambda {
                    binder: binder.replace(index, subst),
                    body: Box::new(body.replace(index + 1, subst)),
                    span: span,
                }
            }
            &Type => Type,
        }
    }

    // pub fn is_closed(&self) -> bool {
    //     use self::Term::*;
    //     use super::Name::*;
    //
    //     match self {
    //         &Var { ref name } => {
    //             match name {
    //                 &DeBruijn { .. } | &Qual { .. } | &Meta { .. } => true,
    //                 &Local { .. } => true,
    //             }
    //         }
    //         &App { ref fun, ref arg, .. } => fun.is_closed() && arg.is_closed(),
    //         &Forall { ref ty, ref term, .. } => ty.is_closed() && term.is_closed(),
    //         &Lambda { ref ty, ref body,.. } => ty.is_closed() && body.is_closed(),
    //         &Recursor(..) => true,
    //         &Literal { .. } | &Type => true,
    //     }
    // }

    pub fn apply(t: Term, u: Term) -> Term {
        Term::App {
            fun: Box::new(t),
            arg: Box::new(u),
            span: Span::dummy(),
        }
    }

    pub fn apply_all(fun: Term, args: Vec<Term>) -> Term {
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

    // fn eta_expand(&self) -> Term {
    //     let mut pi = self;
    //     let mut result = self.inductive_ty.name.to_term();
    //     let mut i = 0;
    //     let mut locals = vec![];
    //     while let Term::Forall { ty, term, .. } = pi {
    //         let local = self.ty_cx.local_with_repr(format!("x{}", 0), *ty);
    //         locals.push(local);
    //         pi = *term;
    //     }
    //     Term::abstract_lambda(
    //         locals.clone(),
    //         Term::apply_all(result, locals.into_iter().map(|t| t.to_term()).collect()))
    // }

    // // Actually eval without unfolding
    // pub fn whnf(&self) -> Term {
    //     use self::Term::*;
    //
    //     match self {
    //         &App { ref fun, ref arg, span } => {
    //             Term::App {
    //                 fun: Box::new(fun.whnf()),
    //                 arg: Box::new(arg.whnf()),
    //                 span: span,
    //             }
    //         }
    //         // &Forall { ref name, ref ty, ref term, span } => {
    //         //     Term::Forall {
    //         //         name: name.clone(),
    //         //         ty: Box::new(ty.whnf()),
    //         //         term: Box::new(term.whnf()),
    //         //         span: span,
    //         //     }
    //         // }
    //         &Lambda { ref name, ref ty, ref body, span } => {
    //             Term::Lambda {
    //                 name: name.clone(),
    //                 ty: Box::new(ty.whnf()),
    //                 body: Box::new(body.whnf()),
    //                 span: span,
    //             }
    //         }
    //         &Recursor(..) => panic!(),
    //         t => t.clone(),
    //     }
    // }

    /// Check if we are stuck returning the meta-variable which is
    /// the head of the stuck term or recursor.
    pub fn is_stuck(&self) -> Option<Name> {
        match self {
            t => t.head().and_then(|h| match &h {
                &Term::Var { ref name } => match name {
                    m @ &Name::Meta { .. } => Some(m.clone()),
                    _ => None
                },
                _ => None
            }),
        }
    }

    /// Will return the "head" of a term
    pub fn head(&self) -> Option<Term> {
        use self::Term::*;
        match self {
            &App { ref fun, .. } => {
                let mut result: &Term = &**fun;
                while let &App { ref fun, .. } = result {
                    result = &**fun;
                }
                Some(result.clone())
            }
            f @ &Forall { .. } => Some(f.clone()),
            l @ &Lambda { .. } => Some(l.clone()),
            v @ &Var { .. } => Some(v.clone()),
            &Type => Some(Type),
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
            }
            &Var { .. } | &Type => Some(vec![]),
            _ => None,
        }
    }

    pub fn uncurry(&self) -> (Term, Vec<Term>) {
        use self::Term::*;

        match self {
            &App { ref fun, ref arg, ..} => {
                let mut f = &**fun;
                let mut result = vec![*arg.clone()];
                while let &App { ref fun, ref arg, .. } = f {
                    f = &**fun;
                    result.push(*arg.clone());
                }
                (f.clone(), result.into_iter().rev().collect())
            }
            t => (t.clone(), vec![])
        }
    }

    pub fn is_sort(&self) -> bool {
        match self {
            &Term::Type => true,
            _ => false,
        }
    }

    pub fn is_forall(&self) -> bool {
        match self {
            &Term::Forall {..} => true,
            _ => false,
        }
    }

    pub fn is_meta(&self) -> bool {
        match self {
            &Term::Var { ref name, .. } =>
                name.is_meta(),
            _ => false,
        }
    }

    pub fn is_app(&self) -> bool {
        match self {
            &Term::App { .. } => true,
            _ => false,
        }
    }

    pub fn head_is_local(&self) -> bool {
        self.head().map(|h| match &h {
            &Term::Var { ref name, .. } => match name {
                &Name::Local { .. } => true,
                _ => false
            },
            _ => false
        }).unwrap_or(false)
    }

    pub fn head_is_global(&self) -> bool {
        self.head().map(|h| match &h {
            &Term::Var { ref name, .. } => match name {
                &Name::Qual { .. } => true,
                _ => false
            },
            _ => false
        }).unwrap_or(false)
    }

    pub fn is_constant(&self) -> bool {
        // Get at the term's head. 
        let (head, args) = self.uncurry();

        // If this is an application it's not a constant
        if args.len() > 0 {
            return false;
        }

        // Otherwise just peak at the head of the term, since there are no args,
        // and see if it is a constant.
        match head {
            // TODO: what does it mean to be a constant
            Term::Var { name } => name.is_local() || name.is_qual(),
            _ => false
        }
    }

    pub fn instantiate_meta(&self, meta: &Name, term: &Term) -> Term {
        let mut result = self.clone();
        result.replace_term(term, &|tm| tm == &meta.to_term());
        result
    }

    pub fn binders(&self) -> Option<Vec<&Term>> {
        let mut cursor = self;
        let mut binders = vec![];

        while let &Term::Forall { ref binder, ref term, .. } = cursor {
            binders.push(&*binder.ty);
            cursor = &**term;
        }

        if binders.len() > 0 {
            return Some(binders);
        }

        // We didn't get the bindings from a forall, so let's try a lambda.
        while let &Term::Lambda { ref binder, ref body, .. } = cursor {
            binders.push(&*binder.ty);
            cursor = &**body;
        }

        if binders.len() > 0 {
            return Some(binders);
        } else {
            return None;
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
                &mut Forall { ref mut binder, ref mut term, .. } => {
                    binder.ty.replace_term(&replacement, pred);
                    term.replace_term(&replacement, pred);
                }
                &mut Lambda { ref mut binder, ref mut body, .. } => {
                    binder.ty.replace_term(&replacement, pred);
                    body.replace_term(&replacement, pred);
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
            (&Var { name: ref name1, .. },
             &Var { name: ref name2, .. }) =>
                name1 == name2,
            (&App { fun: ref fun1, arg: ref arg1, .. },
             &App { fun: ref fun2, arg: ref arg2, .. }) =>
                fun1 == fun2 && arg1 == arg2,
            (&Forall { binder: ref binder1, term: ref term1, .. },
             &Forall { binder: ref binder2, term: ref term2, .. }) => {
                binder1 == binder2 && term1 == term2
            }
            (&Lambda { binder: ref binder1, body: ref body1, .. },
             &Lambda { binder: ref binder2, body: ref body2, ..}) => {
                binder1 == binder2 && body1 == body2
            }
            (&Type, &Type) => true,
            _ => false,
        }
    }
}

impl Hash for Term {
    fn hash<H>(&self, state: &mut H)
        where H: Hasher
    {
        use self::Term::*;
        debug!("hash: {}", self);

        match self {
            &Var { ref name, .. } => {
                0.hash(state);
                name.hash(state);
            }
            &App { ref fun, ref arg, .. } => {
                1.hash(state);
                fun.hash(state);
                arg.hash(state);
            }
            &Forall { ref binder, ref term, .. } => {
                2.hash(state);
                binder.hash(state);
                term.hash(state);
            }
            &Lambda { ref binder, ref body, .. } => {
                3.hash(state);
                binder.hash(state);
                body.hash(state);
            }
            &Type => {
                4.hash(state);
            }
        }
    }
}

impl Pretty for Term {
    fn pretty(&self) -> Doc {
        use self::Term::*;

        match self {
            &Var { ref name, .. } => name.pretty(),
            &App { ref fun, ref arg, .. } => {
                let pretty_fun = match &**fun {
                    complex @ &Term::Lambda { .. } =>
                        parens(complex.pretty()),
                    t => t.pretty()
                };

                match &**arg {
                    &Term::App { .. } => pretty_fun + " ".pretty() + parens(arg.pretty()),
                    _ => pretty_fun + " ".pretty() + arg.pretty(),
                }
            }
            &Forall { ref binder, ref term, .. } => {
                if binder.name.is_placeholder() {
                    let p = match &*binder.ty {
                        &Forall {..} => parens(binder.ty.pretty()) + " -> ".pretty(),
                        _ => binder.ty.pretty() + " -> ".pretty(),
                    };
                    p + term.pretty()
                } else {
                    let mut cursor = &**term;
                    let mut binders = Vec::new();
                    binders.push(binder);
                    while let &Term::Forall { ref binder, ref term, .. } = cursor {
                        // This is because we only want to pretty print the chunk of
                        // binders up to a placeholder name.
                        if binder.name.is_placeholder() { break; }
                        binders.push(binder);
                        cursor = term;
                    }
                    "forall ".pretty() + pretty_binders(binders.as_slice()) +
                        ", ".pretty() + cursor.pretty()
                }
            }
            &Lambda { ref binder, ref body, .. } => {
                // // This will be the term we will unroll binders from.
                // let mut term = &**body;
                //
                // // A list of coalesced binders
                // let mut cbinders = vec![];
                //
                // // Store the first binder's type
                // let mut binder_ty = &binder.ty;
                // let mut binders = vec![binder];
                // // If there is a sequence of binders then we want to coalesce
                // // them when printing like we can do in the syntax. The below
                // // loop will collect said binders.
                // while let &Term::Lambda { ref binder, ref body, .. } = term {
                //     if binder.ty == *binder_ty {
                //         binders.push(binder);
                //         term = &*body;
                //     } else {
                //         cbinders.push((binders, binder_ty));
                //         binder_ty = &binder.ty;
                //         binders = vec![];
                //     }
                // }
                //
                // // This code is ugly, lo
                // if cbinders.len() == 0 {
                //     cbinders.push((binders, binder_ty));
                // }
                //
                // // I think this code could probably be cleaner.
                // let mut coalesced_binder = "".pretty();
                // for (binders, ty) in cbinders {
                //     coalesced_binder = coalesced_binder + "(".pretty();
                //     for binder in binders {
                //         coalesced_binder = coalesced_binder + binder.name.pretty() + " ".pretty();
                //     }
                //     coalesced_binder = coalesced_binder + ": ".pretty() + ty.pretty();
                //     coalesced_binder = coalesced_binder + ") ".pretty();
                // }
                // Now we pretty print the function with the collesced binders.
                let mut cursor = &**body;
                let mut binders = Vec::new();
                binders.push(binder);
                while let &Term::Lambda { ref binder, ref body, .. } = cursor {
                    // This is because we only want to pretty print the chunk of
                    // binders up to a placeholder name.
                    if binder.name.is_placeholder() { break; }
                    binders.push(binder);
                    cursor = body;
                }

                "fun ".pretty() + pretty_binders(binders.as_slice()) + " => ".pretty() + cursor.pretty()
            }
            &Type => Doc::text("Type"),
        }
    }
}

// impl Display for Option<Term> {
//     fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
//         match self {
//             &None => write!(formatter, "None"),
//             &Some(v) => write!(formatter, "Some({})", v),
//         }
//     }
// }

impl Display for Term {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        format(self, formatter)
    }
}

impl HasSpan for Term {
    fn get_span(&self) -> Span {
        use self::Term::*;

        match self {
            &Var { ref name } => name.get_span(),
            &App { span, .. } => span,
            &Forall { span, .. } => span,
            &Lambda { span, .. } => span,
            &Type => Span::dummy(),
        }
    }

    fn set_span(&mut self, sp: Span) {
        use self::Term::*;

        match self {
            &mut Var { ref mut name } => name.set_span(sp),
            &mut App { ref mut span, .. } => *span = sp,
            &mut Forall { ref mut span, .. } => *span = sp,
            &mut Lambda { ref mut span, .. } => *span = sp,
            &mut Type => {}
        }
    }
}
