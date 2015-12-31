mod error_reporting;

use core::*;
use super::ast::{SourceMap, Span, HasSpan};

// Re-exports
pub use self::error_reporting::*;

use std::collections::HashMap;
use std::path::PathBuf;

#[derive(Debug, Clone)]
pub enum Error {
    ApplicationErr,
    UnificationErr(Span, Term, Term),
    UnknownVariable(Name),
    ElaborationError,
    MkErr
}

/// A global context for type checking containing the necessary information
/// needed across type checking all definitions.
pub struct TyCtxt {
    types: HashMap<Name, Data>,
    functions: HashMap<Name, Function>,
    globals: HashMap<Name, Term>,
    source_map: SourceMap,
}

#[derive(Clone)]
pub struct LocalCx<'tcx> {
    ty_cx: &'tcx TyCtxt,
    locals: HashMap<Name, Term>,
    // I think this should be more flexible
    equalities: HashMap<Term, Term>
}

impl TyCtxt {
    pub fn empty() -> TyCtxt {
        TyCtxt {
            types: HashMap::new(),
            functions: HashMap::new(),
            globals: HashMap::new(),
            source_map: SourceMap::new(PathBuf::new(), "".to_string()),
        }
    }

    pub fn from_module(module: &Module, source_map: SourceMap) -> TyCtxt {
        let mut tycx = TyCtxt::empty();
        tycx.source_map = source_map;

        for def in &module.defs {
            match def {
                &Definition::Data(ref d) => {
                    tycx.types.insert(d.name.clone(), d.clone());
                    tycx.globals.insert(d.name.clone(), d.ty.clone());
                    for ctor in &d.ctors {
                        tycx.globals.insert(
                            ctor.0.clone(),
                            ctor.1.clone());
                    }
                }
                &Definition::Fn(ref f) => {
                    tycx.functions.insert(f.name.clone(), f.clone());
                    tycx.globals.insert(f.name.clone(), f.ty());
                }
                &Definition::Extern(ref e) => {
                    tycx.globals.insert(e.name.clone(), e.term.clone());
                }
            }
        }

        return tycx;
    }


    pub fn type_check_def(&self, def: &Definition) -> Result<(), Error> {
        debug!("type_check_def: def={:?}", def);
        match def {
            &Definition::Fn(ref fun) => {
                let &Function {
                    ref args,
                    ref ty,
                    ref body, ..
                } = fun;

                let mut lcx = LocalCx::from_cx(self);

                for &(ref n, ref arg_ty) in args {
                    debug!("type_check_def: checking args={:?}", args);
                    try!(lcx.type_check_term(arg_ty, &Term::Type));
                    lcx = lcx.extend(vec![(n.clone(), arg_ty.clone())]);
                }

                try!(lcx.type_check_term(&body, &ty));
                Ok(())
            }
            _ => Ok(())
        }
    }
}

impl<'tcx> LocalCx<'tcx> {
    fn from_cx(ty_cx: &'tcx TyCtxt) -> LocalCx<'tcx> {
        LocalCx {
            ty_cx: ty_cx,
            locals: HashMap::new(),
            equalities: HashMap::new(),
        }
    }

    fn datatype(&self, ty: &Term) -> Option<&Data> {
        match ty {
            &Term::Var { ref name } => {
                self.ty_cx.types.get(name)
            }
            _ => None
        }
    }

    fn ctors(&self, ty: &Term) -> Option<Vec<(Name, Term)>> {
        self.datatype(ty).map(|dt| {
            dt.ctors.clone()
        })
    }

    fn extend(mut self, bindings: Vec<(Name, Term)>) -> LocalCx<'tcx> {
        for (x, t) in bindings {
            self.locals.insert(x, t);
        }

        self
    }

    fn lookup(&self, name: &Name) -> Result<&Term, Error> {
        match self.locals.get(name) {
            None => match self.ty_cx.globals.get(name) {
                None => Err(Error::UnknownVariable(name.clone())),
                Some(t) => Ok(t)
            },
            Some(t) => Ok(t)
        }
    }

    // This is super ugly, come back and do a second pass, sketching.
    pub fn unify(&self, span: Span, t: &Term, u: &Term) -> Result<Term, Error> {
        debug!("unify: {} {}", t, u);
        if t == u {
            Ok(t.clone())
        } else {
            for (x, y) in &self.equalities {
                debug!("{} = {}", x, y);
            }
            let mut needed_eqs = vec![];
            equal_modulo(t, u, &mut needed_eqs);
            for &(ref x, ref y) in &needed_eqs {
                match self.equalities.get(x) {
                    None =>
                        return Err(Error::UnificationErr(span, t.clone(), u.clone())),
                    Some(yp) => {
                        if y != yp {
                            return Err(Error::UnificationErr(span, t.clone(), u.clone()));
                        }
                    }
                }
            }

            return Ok(t.clone());
        }
    }

    pub fn type_check_term(&self, term: &Term, ty: &Term) -> Result<Term, Error> {
        match term {
            &Term::Match { ref scrutinee, ref cases, .. } => {
                let scrut_ty = try!(self.type_infer_term(scrutinee));
                let ctors: HashMap<_, _> = self.ctors(&scrut_ty).unwrap().into_iter().collect();
                for case in cases.iter() {
                    match &case.pattern {
                        &Pattern::Simple(ref simple_pat) => match simple_pat {
                            &SimplePattern::Name(ref n) => {
                                let mut cx = self.clone()
                                                 .extend(vec![(n.clone(), scrut_ty.clone())]);
                                let pat_term = case.pattern.to_term();
                                cx.equalities.insert(*scrutinee.clone(), pat_term);
                                try!(cx.type_check_term(&case.rhs, ty));
                            }
                            &SimplePattern::Placeholder => {
                                try!(self.type_check_term(&case.rhs, ty));
                            }
                        },
                        &Pattern::Constructor(ref n, ref patterns) => {
                            let pat_term = case.pattern.to_term();
                            let mut cx = self.clone();
                            cx.equalities.insert(*scrutinee.clone(), pat_term);
                            if patterns.len() == 0 {
                                try!(cx.type_infer_term(&case.rhs));
                            } else {
                                let ctor = ctors.get(n).unwrap();
                                try!(cx.bind_pattern(ctor, patterns, |cx| {
                                    cx.type_check_term(&case.rhs, ty)
                                }));
                            }
                        }
                    }
                }

                Ok(ty.clone())
            },
            &Term::Lambda { .. } => panic!("can't type check lambda's yet"),
            _ => {
                debug!("type_check_term: infering the type of {}", term);
                let infer_ty = try!(self.type_infer_term(term));
                debug!("type_check_term: checking {} againist the inferred type {}",
                    ty,
                    infer_ty);
                let term = try!(self.unify(term.get_span(), ty,  &infer_ty));
                debug!("return from unify");
                Ok(term)
            }
        }
    }

    pub fn type_infer_term(&self, term: &Term) -> Result<Term, Error> {
        match term {
            &Term::Literal { ref lit, .. } => match lit {
                &Literal::Int(..) => Ok(panic!()),
                &Literal::Unit => Ok(Term::Var {
                    name: Name::from_str("Unit")
                })
            },
            &Term::Var { ref name, .. } => Ok(try!(self.lookup(name)).clone()),
            &Term::App { ref fun, ref arg, .. } => {
                debug!("inside app {:?} {}", fun, arg);
                match try!(self.type_infer_term(fun)) {
                    Term::Forall { name, ty, term, .. } => {
                        debug!("inside forall: `{}` `{}` `{}`", name, ty, term);
                        try!(self.type_check_term(arg, &*ty));
                        // try!(self.evaluate())
                        Ok(term.subst(&name, arg))
                    },
                    _ => Err(Error::ApplicationErr),
                }
            }
            &Term::Forall { ref name, ref ty, ref term, .. } => {
                try!(self.type_check_term(&*ty, &Term::Type));
                let cx = self.clone().extend(vec![(name.clone(), *ty.clone())]);
                try!(cx.type_check_term(&*term, &Term::Type));
                Ok(Term::Type)
            }
            &Term::Type => Ok(Term::Type),
            _ => panic!("can only check type"),
        }
    }

    pub fn bind_pattern<R, F : FnOnce(LocalCx) -> R>(
        &self,
        ty: &Term,
        patterns: &Vec<SimplePattern>,
        f: F) -> R {
        debug!("bind_pattern: {:?}", ty);
        let mut quantifier = ty.clone();
        let mut quantifier_level = 0;
        let mut cx = self.clone();

        while quantifier_level < patterns.len() {
            /* this is mostly definitely not right */
            quantifier = match quantifier {
                Term::Forall { name, ty, term, .. } => {
                    match &patterns[quantifier_level] {
                        &SimplePattern::Name(ref n) => {
                            debug!("extending {} mapsto {:?}", n, ty);
                            cx = cx.extend(vec![(n.clone(), *ty.clone())]);
                            term.subst(&name, &Term::Var { name: n.clone() })
                        }
                        // Not sure about this case
                        &SimplePattern::Placeholder => {
                            *term
                        }
                    }

                },
                _ => panic!("invalid pattern binding")
            };

            quantifier_level += 1;
        }

        f(cx)
    }

    pub fn evaluate(&self, term: &Term) -> Term {
        term.clone()
    }
}

fn equal_modulo(t1: &Term, t2: &Term, equalities: &mut Vec<(Term, Term)>) -> bool {
    use core::Term::*;

    debug!("equal_modulo: {} == {}", t1, t2);

    match (t1, t2) {
        (&Match { .. },
         &Match {..}) => panic!(),
        (&App { fun: ref fun1, arg: ref arg1, .. },
         &App { fun: ref fun2, arg: ref arg2, .. }) =>
            equal_modulo(fun1, fun2, equalities) &&
            equal_modulo(arg1, arg2, equalities),
        (&Forall { name: ref name1, ty: ref ty1, term: ref term1, .. },
         &Forall { name: ref name2, ty: ref ty2, term: ref term2, .. }) =>
            equal_modulo(&name1.to_term(), &name2.to_term(), equalities) &&
            equal_modulo(ty1, ty2, equalities) &&
            equal_modulo(term1, term2, equalities),
        (&Lambda { args: ref args1, ret_ty: ref ret_ty1, body: ref body1, .. },
         &Lambda { args: ref args2, ret_ty: ref ret_ty2, body: ref body2, ..}) =>
            args1.iter().zip(args2.iter()).all(|(a1, a2)|
                equal_modulo(&a1.0.to_term(), &a2.0.to_term(), equalities) &&
                equal_modulo(&a1.1, &a2.1, equalities)) &&
            equal_modulo(ret_ty1, ret_ty2, equalities) &&
            equal_modulo(body1, body2, equalities),
        (t, u) => {
            if t == u {
                true
            } else {
                equalities.push((t.clone(), u.clone()));
                false
            }
        }
    }
}

// struct EvalCx<'lcx, 'tcx> {
//     local_cx: &'lcx LocalCx<'tcx>,
// }
//
// impl<'lcx, 'tcx> EvalCx<'lcx, 'tcx> {
//
// }
