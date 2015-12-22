use std::collections::HashMap;

use core::*;

#[derive(Debug, Clone)]
pub enum Error {
    ApplicationErr,
    UnificationErr(Term, Term),
    UnknownVariable(Name),
    MkErr
}

/// A global context for type checking containing the necessary information
/// needed across type checking all definitions.
pub struct TyCtxt {
    types: HashMap<Name, Data>,
    functions: HashMap<Name, Function>,
    globals: HashMap<Name, Term>
}

#[derive(Clone)]
pub struct LocalCx<'tcx> {
    ty_cx: &'tcx TyCtxt,
    locals: HashMap<Name, Term>
}

impl TyCtxt {
    pub fn empty() -> TyCtxt {
        TyCtxt {
            types: HashMap::new(),
            functions: HashMap::new(),
            globals: HashMap::new(),
        }
    }

    pub fn from_module(module: &Module) -> TyCtxt {
        let mut tycx = TyCtxt::empty();
        for def in &module.defs {
            match def {
                &Definition::Data(ref d) => {
                    tycx.types.insert(d.name.clone(), d.clone());
                    // TODO: add each constructor here
                }
                &Definition::Fn(ref f) => {
                    tycx.functions.insert(f.name.clone(), f.clone());
                    tycx.globals.insert(f.name.clone(), f.ty());
                }
                &Definition::Extern(ref e) => {
                    tycx.globals.insert(e.0.clone(), e.1.clone());
                }
            }
        }

        return tycx;
    }


    pub fn type_check_def(&self, def: &Definition) -> Result<(), Error> {
        match def {
            &Definition::Fn(ref fun) => {
                let lcx = LocalCx::from_cx(self)
                                  .extend(fun.args.clone());
                try!(lcx.type_check_term(&fun.body, &fun.ty));
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
        }
    }

    fn datatype(&self, ty: &Term) -> Option<&Data> {
        match ty {
            &Term::Var(ref n) => {
                self.ty_cx.types.get(n)
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

    pub fn unify(&self, t: &Term, u: &Term) -> Result<Term, Error> {
        if t == u {
            Ok(t.clone())
        } else {
            Err(Error::UnificationErr(t.clone(), u.clone()))
        }
    }

    pub fn type_check_term(&self, term: &Term, ty: &Term) -> Result<Term, Error> {
        let infer_ty = try!(self.type_infer_term(term));
        self.unify(&infer_ty, ty)
        // match term {
        //     &Term::Literal(ref lit) => self.type_check_lit(lit, ty),
        //     &Term::Var(ref n) => self.unify(self.lookup(n), ty),
        //     &Term::Match(..) => panic!(),
        //     &Term::App(ref f, ref g) => {
        //         let g_ty = try!(self.type_infer_term(g));
        //         let f_ty = try!(self.type_infer_term(f));
        //         println!("typechecking app: {:?} {:?}", g_ty, f_ty);
        //         Err(Error::MkErr)
        //     }
        //     &Term::Forall(..) => panic!(),
        //     &Term::Lambda(..) => panic!(),
        //     &Term::Type => self.unify(&Term::Type, ty),
        // }
    }

    pub fn type_infer_term(&self, term: &Term) -> Result<Term, Error> {
        match term {
            &Term::Literal(ref lit) => match lit {
                &Literal::Int(..) => Ok(panic!()),
                &Literal::Unit => Ok(Term::Var("Unit".to_string())),
            },
            &Term::Var(ref n) => Ok(try!(self.lookup(n)).clone()),
            &Term::Match(ref scrut, ref cases) => {
                let scrut_ty = try!(self.type_infer_term(scrut));
                let ctors: HashMap<_, _> = self.ctors(&scrut_ty).unwrap().into_iter().collect();
                let arm_types = cases.iter().map(|case| {
                    match &case.pattern {
                        &Pattern::Name(ref n) => {
                            let cx = self.clone();
                            let cx = cx.extend(vec![(n.clone(), scrut_ty.clone())]);
                            cx.type_infer_term(&case.rhs)
                        }
                        &Pattern::Constructor(ref n, ref patterns) => {
                            if patterns.len() == 0 {
                                self.type_infer_term(&case.rhs)
                            } else {
                                let ctor = ctors.get(n).unwrap();
                                self.bind_pattern(ctor, patterns, |cx| {
                                    cx.type_infer_term(&case.rhs)
                                })
                            }
                        }
                        &Pattern::Placeholder => {
                            self.type_infer_term(&case.rhs)
                        }
                    }
                }).collect::<Vec<_>>();
                arm_types[0].clone()
            }
            &Term::App(ref f, ref g) => {
                match try!(self.type_infer_term(f)) {
                    Term::Forall(n, arg_ty, body_ty) => {
                        try!(self.type_check_term(g, &*arg_ty));
                        // try!(self.evaluate())
                        Ok(*body_ty)
                    },
                    _ => Err(Error::ApplicationErr),
                }
            }
            &Term::Forall(..) => panic!(),
            &Term::Lambda(..) => panic!(),
            &Term::Type => Ok(Term::Type),
        }
    }

    pub fn bind_pattern<R, F : FnOnce(LocalCx) -> R>(
        &self,
        ty: &Term,
        patterns: &Vec<Pattern>,
        f: F) -> R {
        let mut ty = ty;
        let mut pat_start = 0;

        /* this is mostly definitely not right */
        match ty {
            &Term::Forall(ref n, ref dom, ref codom) => {
                let cx = self.clone();
                f(cx.extend(vec![(n.clone(), *dom.clone())]))
            },
            _ => panic!("invalid pattern binding")
        }
    }

    pub fn evaluate(&self, term: &Term) -> Term {
        term.clone()
    }
}
