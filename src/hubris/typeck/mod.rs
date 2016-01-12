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
    UnificationErr(Span, Term, Term, Vec<(Term, Term)>),
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
    pub source_map: SourceMap,
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
        let mut ty_cx = TyCtxt::empty();
        ty_cx.source_map = source_map;

        for def in &module.defs {
            match def {
                &Definition::Data(ref d) =>
                    ty_cx.declare_datatype(d),
                &Definition::Fn(ref f) =>
                    ty_cx.declare_def(f),
                &Definition::Extern(ref e) =>
                    ty_cx.declare_extern(e),
            }
        }

        return ty_cx;
    }

    pub fn declare_datatype(&mut self, data_type: &Data) {
        self.types.insert(data_type.name.clone(), data_type.clone());
        self.globals.insert(data_type.name.clone(), data_type.ty.clone());
        for ctor in &data_type.ctors {
            self.globals.insert(
                ctor.0.clone(),
                ctor.1.clone());
        }
    }

    pub fn declare_def(&mut self, f: &Function) {
        self.functions.insert(f.name.clone(), f.clone());
        self.globals.insert(f.name.clone(), f.ty());
    }

    pub fn declare_extern(&mut self, e: &Extern) {
        self.globals.insert(e.name.clone(), e.term.clone());
    }

    pub fn type_check_def(&self, def: &Definition) -> Result<(), Error> {
        debug!("type_check_def: def={}", def);
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
            let mut inequalities = vec![];
            equal_modulo(t, u, &mut inequalities);
            Err(Error::UnificationErr(span, t.clone(), u.clone(), inequalities))
        }
    }

    pub fn type_check_term(&self, term: &Term, ty: &Term) -> Result<Term, Error> {
        debug!("type_check_term: infering the type of {}", term);
        let infer_ty = try!(self.type_infer_term(term));
        debug!("type_check_term: checking {} againist the inferred type {}",
                ty,
                infer_ty);
        let term = try!(self.unify(term.get_span(), ty,  &infer_ty));
        debug!("return from unify");
        Ok(term)
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
                match try!(self.type_infer_term(fun)) {
                    Term::Forall { name, ty, term, .. } => {
                        try!(self.type_check_term(arg, &*ty));
                        // try!(self.evaluate())
                        Ok(term.subst(name.to_index(), arg))
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
            &Term::Lambda { ref args, ref ret_ty, ref body, span, } => {
                let lcx = self.clone()
                              .extend(args.clone());

                try!(lcx.type_check_term(body, ret_ty));

                let mut pi_type = *body.clone();

                for &(ref n, ref t) in args.iter().rev() {
                    pi_type = Term::Forall {
                        span: span,
                        name: n.clone(),
                        ty: Box::new(t.clone()),
                        term: Box::new(pi_type),
                    }
                }

                Ok(pi_type)
            }
            &Term::Type => Ok(Term::Type),
        }
    }

    pub fn evaluate(&self, term: &Term) -> Term {
        term.clone()
    }
}

fn equal_modulo(t1: &Term, t2: &Term, equalities: &mut Vec<(Term, Term)>) -> bool {
    use core::Term::*;

    debug!("equal_modulo: {} == {}", t1, t2);

    match (t1, t2) {
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
