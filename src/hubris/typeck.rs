use std::collections::HashMap;

use core::*;

pub enum Error {
    MkErr
}

/// A global context for type checking containing the necessary information
/// needed across type checking all definitions.
pub struct TyCtxt {
    types: HashMap<Name, Data>,
    functions: HashMap<Name, Function>
}

pub struct LocalCx<'tcx> {
    ty_cx: &'tcx TyCtxt,
    locals: HashMap<Name, Term>
}

impl TyCtxt {
    pub fn empty() -> TyCtxt {
        TyCtxt {
            types: HashMap::new(),
            functions: HashMap::new(),
        }
    }

    pub fn from_module(module: &Module) -> TyCtxt {
        let mut tycx = TyCtxt::empty();
        for def in &module.defs {
            match def {
                &Definition::Data(ref d) => {
                    tycx.types.insert(d.name.clone(), d.clone());
                }
                &Definition::Fn(ref f) => {
                    tycx.functions.insert(f.name.clone(), f.clone());
                }
                &Definition::Extern(ref e) => {
                    panic!()
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

    fn extend(mut self, bindings: Vec<(Name, Term)>) -> LocalCx<'tcx> {
        for (x, t) in bindings {
            self.locals.insert(x, t);
        }

        self
    }

    fn lookup(&self, name: &Name) -> &Term {
        match self.locals.get(name) {
            None => panic!("can't find local variable"),
            Some(t) => t
        }
    }

    pub fn unify(&self, t: &Term, u: &Term) -> Result<Term, Error> {
        if t == u {
            Ok(t.clone())
        } else {
            Err(Error::MkErr)
        }
    }

    pub fn type_check_term(&self, term: &Term, ty: &Term) -> Result<Term, Error> {
        match term {
            &Term::Literal(ref lit) => self.type_check_lit(lit, ty),
            &Term::Var(ref n) => self.unify(self.lookup(n), ty),
            &Term::Match(..) => panic!(),
            &Term::App(ref f, ref g) => {
                // let g_ty = try!(self.type_infer_term(g));
                // let f_ty = try!(self.type_infer_term(f));
                // println!("{:?} {:?}", g_ty, f_ty);
                Err(Error::MkErr)
            }
            &Term::Forall(..) => panic!(),
            &Term::Lambda(..) => panic!(),
            &Term::Type => self.unify(&Term::Type, ty),
        }
    }

    pub fn type_infer_term(&self, term: &Term) -> Result<Term, Error> {
        match term {
            &Term::Literal(ref lit) => match lit {
                &Literal::Int(..) => Ok(panic!()),
                &Literal::Unit => Ok(panic!()),
            },
            &Term::Var(ref n) => Ok(self.lookup(n).clone()),
            &Term::Match(..) => panic!(),
            &Term::App(ref f, ref g) => panic!(),
            &Term::Forall(..) => panic!(),
            &Term::Lambda(..) => panic!(),
            &Term::Type => Ok(Term::Type),
        }
    }

    pub fn type_check_lit(&self, term: &Literal, ty: &Term) -> Result<Term, Error> {
        panic!()
    }

    pub fn evaluate(&self, term: &Term) -> Term {
        term.clone()
    }
}
