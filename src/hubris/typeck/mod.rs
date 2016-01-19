mod error_reporting;

use core::*;
use super::ast::{SourceMap, Span, HasSpan};

// Re-exports
pub use self::error_reporting::*;

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::PathBuf;
use std::iter;

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
    // We keep these around right now, but I'm not sure if we should.
    types: HashMap<Name, Data>,
    functions: HashMap<Name, Function>,

    axioms: HashMap<Name, Term>,
    definitions: HashMap<Name, (Term, Term)>,

    pub source_map: SourceMap,

    local_counter: RefCell<usize>,
}

impl TyCtxt {
    pub fn empty() -> TyCtxt {
        TyCtxt {
            types: HashMap::new(),
            functions: HashMap::new(),
            axioms: HashMap::new(),
            definitions: HashMap::new(),
            source_map: SourceMap::new(PathBuf::new(), "".to_string()),
            local_counter: RefCell::new(0),
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
        // Currently we use types/functions for metadata, do we need them?
        self.types.insert(data_type.name.clone(), data_type.clone());

        // The type is just a constant with the type `ty`
        self.axioms.insert(data_type.name.clone(), data_type.ty.clone());

        // Each constructor also becomes a constant with type ascribed
        // in its definition.
        for ctor in &data_type.ctors {
            let name = ctor.0.clone();
            let ty = ctor.1.clone();
            self.axioms.insert(name, ty);
        }

        // Finally we will build a recursor for the type.
        // Parameters should come first, but we don't have them yet.

        // We then create ...

        let local_c = self.local_with_repr(
            "C".to_string(),
            Term::abstract_pi(
                vec![self.local_with_repr("".to_string(), data_type.name.clone().to_term())],
                Term::Type));

        let mut premises = Vec::new();

        for &(ref name, ref ty) in &data_type.ctors {
            let mut ctor_ty = ty;

            let mut premise_args = Vec::new();
            let mut names = Vec::new();


            while let &Term::Forall { ref name, ref ty, ref term, .. } = ctor_ty {
                let local_n = self.local_with_repr(
                    "n".to_string(),
                    *ty.clone());

                premise_args.push(local_n.clone());

                let local_x = self.local_with_repr(
                    "".to_string(),
                    Term::apply(local_c.to_term(), local_n.to_term()));

                premise_args.push(local_x);

                if true {
                    names.push(local_n.clone().to_term());
                }

                ctor_ty = term;
            }

            let c_for_ctor = Term::apply(
                local_c.to_term(),
                Term::apply_all(name.to_term(), names));

            let premise = Term::abstract_pi(premise_args, c_for_ctor);

            premises.push(premise);
        }

        let premises = premises.into_iter()
                               .map(|p| self.local_with_repr("".to_string(), p))
                               .collect();

        let mut result = data_type.ty.clone();

        let mut tys = Vec::new();
        while let Term::Forall { name, ty, term, .. } = result {
            tys.push(*ty.clone());
            result = *term;
        }

        tys.push(data_type.name.to_term());

        let tys: Vec<_> = tys
                     .into_iter()
                     .enumerate()
                     .map(|(i, ty)| self.local_with_repr(format!("a{}", i), ty))
                     .collect();

        let tys_terms = tys.iter().map(|t| t.to_term()).collect();

        let recursor_ty = Term::abstract_pi(
            vec![local_c.clone()],
            Term::abstract_pi(premises,
            Term::abstract_pi(tys,
                Term::apply_all(local_c.to_term(), tys_terms))));

        println!("declare_datatype: recursor_ty={}", recursor_ty);

        self.definitions.insert(Name::from_str("Nat_rec"),
            (recursor_ty, Term::Recursor(data_type.name.clone())));
    }

    pub fn declare_def(&mut self, f: &Function) {
        self.functions.insert(f.name.clone(), f.clone());
        self.definitions.insert(f.name.clone(), (f.ty(), f.body.clone()));
    }

    /// Declaring an external function creates an axiom in the type checker
    /// with the appropriate type.
    ///
    /// During code generation we will deal with creating a symbol for this
    /// function.
    pub fn declare_extern(&mut self, e: &Extern) {
        self.axioms.insert(e.name.clone(), e.term.clone());
    }

    pub fn type_check_def(&self, def: &Definition) -> Result<(), Error> {
        debug!("type_check_def: def={}", def);
        match def {
            &Definition::Fn(ref fun) => {
                let &Function {
                    ref args,
                    ref ret_ty,
                    ref body, ..
                } = fun;

                let mut lcx = LocalCx::from_cx(self);

                for &(ref n, ref arg_ty) in args {
                    debug!("type_check_def: checking args={:?}", args);
                    try!(lcx.type_check_term(arg_ty, &Term::Type));
                }

                try!(lcx.type_check_term(&body, &ret_ty));
                Ok(())
            }
            _ => Ok(())
        }
    }

    fn lookup_global(&self, name: &Name) -> Result<&Term, Error> {
        match self.definitions.get(name) {
            None => match self.axioms.get(name) {
                None => Err(Error::UnknownVariable(name.clone())),
                Some(t) => Ok(t)
            },
            Some(t) => Ok(&t.0)
        }
    }

    pub fn local(&self, name: &Name, ty: Term) -> Name {
        let repr = match name {
            &Name::DeBruijn { ref repr, .. } => repr,
            _ => panic!("creating local {:?}", name),
        };

        self.local_with_repr(repr.clone(), ty)
    }

    pub fn local_with_repr(&self, repr: String, ty: Term) -> Name {
        let new_local = Name::Local {
            number: *self.local_counter.borrow(),
            ty: Box::new(ty),
            repr: repr.clone(),
        };

        *self.local_counter.borrow_mut() += 1;

        new_local
    }
}

#[derive(Clone)]
pub struct LocalCx<'tcx> {
    ty_cx: &'tcx TyCtxt,
    // Local entries in the typing context.
    locals: HashMap<Name, Term>,
    // I think this should be more flexible
    equalities: HashMap<Term, Term>,
}

impl<'tcx> LocalCx<'tcx> {
    fn from_cx(ty_cx: &'tcx TyCtxt) -> LocalCx<'tcx> {
        LocalCx {
            ty_cx: ty_cx,
            locals: HashMap::new(),
            equalities: HashMap::new(),
        }
    }

    #[inline]
    pub fn local(&mut self, name: &Name, ty: Term) -> Name {
        self.ty_cx.local(name, ty)
    }

    pub fn def_eq(&self, span: Span, t: &Term, u: &Term) -> Result<Term, Error> {
        debug!("unify: {} {}", t, u);
        let t = t.whnf();
        let u = u.whnf();

        let mut inequalities = vec![];
        let is_def_eq = def_eq_modulo(&t, &u, &mut inequalities);
        if is_def_eq {
            assert_eq!(inequalities.len(), 0);
            Ok(t.clone())
        } else {
            Err(Error::UnificationErr(span, t.clone(), u.clone(), inequalities))
        }
    }

    pub fn type_check_term(&mut self, term: &Term, ty: &Term) -> Result<Term, Error> {
        debug!("type_check_term: infering the type of {}", term);
        let infer_ty = try!(self.type_infer_term(term));
        debug!("type_check_term: checking {} againist the inferred type {}",
                ty,
                infer_ty);
        let term = try!(self.def_eq(term.get_span(), ty,  &infer_ty));
        debug!("return from unify");
        Ok(term)
    }

    pub fn type_infer_term(&mut self, term: &Term) -> Result<Term, Error> {
        match term {
            &Term::Literal { ref lit, .. } => match lit {
                &Literal::Int(..) => Ok(panic!()),
                &Literal::Unit => Ok(Term::Var {
                    name: Name::from_str("Unit")
                })
            },
            &Term::Var { ref name, .. } => match name {
                &Name::Local { ref ty, .. } =>
                        Ok(*ty.clone()),
                q @ &Name::Qual { .. } =>
                    self.ty_cx.lookup_global(q).map(Clone::clone),
                _ => {
                    panic!("internal error: all variable occurences must be free when type checking
                            term that is a variable")
                }
            },
            &Term::App { ref fun, ref arg, .. } => {
                match try!(self.type_infer_term(fun)) {
                    Term::Forall { name, ty, term, .. } => {
                        // try!(self.type_infer_term(arg));
                        Ok(term.instantiate(arg))
                    },
                _ => Err(Error::ApplicationErr),
                }
            }
            &Term::Forall { ref name, ref ty, ref term, .. } => {
                let local = self.local(name, *ty.clone());
                let term = term.instantiate(&local.to_term());

                try!(self.type_check_term(&*ty, &Term::Type));
                try!(self.type_check_term(&term, &Term::Type));

                Ok(Term::Type)
            }
            &Term::Lambda { ref name, ref ty, ref body, span, } => {
                let local = self.local(name, *ty.clone());

                let body = body.instantiate(&local.to_term());

                let pi_body =
                    try!(self.type_infer_term(&body))
                             .abstr(&local);

                Ok(Term::Forall {
                    span: span,
                    name: name.clone(),
                    ty: ty.clone(),
                    term: Box::new(pi_body),
                })
            }
            &Term::Type => Ok(Term::Type),
            _ => panic!(),
        }
    }

    pub fn evaluate(&self, term: &Term) -> Term {
        term.clone()
    }
}

fn def_eq_modulo(t1: &Term, t2: &Term, equalities: &mut Vec<(Term, Term)>) -> bool {
    use core::Term::*;

    debug!("equal_modulo: {} == {}", t1, t2);

    match (t1, t2) {
        (&App { fun: ref fun1, arg: ref arg1, .. },
         &App { fun: ref fun2, arg: ref arg2, .. }) =>
            def_eq_modulo(fun1, fun2, equalities) &&
            def_eq_modulo(arg1, arg2, equalities),
        (&Forall { ty: ref ty1, term: ref term1, .. },
         &Forall { ty: ref ty2, term: ref term2, .. }) =>
            def_eq_modulo(ty1, ty2, equalities) &&
            def_eq_modulo(term1, term2, equalities),
        (&Lambda { ty: ref ty1, body: ref body1, .. },
         &Lambda { ty: ref ty2, body: ref body2, ..}) =>
            def_eq_modulo(ty1, ty2, equalities) &&
            def_eq_modulo(body1, body2, equalities),
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
