use super::TyCtxt;

use super::super::core::*;
use super::name_generator::*;

/// A context with the needed state to deal with operations that process an inductive definition.
pub struct RecursorCx<'i, 'tcx> {
    ty_cx: &'tcx mut TyCtxt,
    inductive_ty: &'i Data,
    ind_hyp: Name,
}

impl<'i, 'tcx> RecursorCx<'i, 'tcx> {
    fn new(ty_cx: &'tcx mut TyCtxt, inductive_ty: &'i Data) -> RecursorCx<'i, 'tcx> {

        let mut rcx = RecursorCx {
            ty_cx: ty_cx,
            inductive_ty: inductive_ty,
            ind_hyp: inductive_ty.name.clone(),
        };

        let ty = rcx.eta_expand();

        let ty = rcx.with_params(ty);
        let ind_hyp = rcx.ty_cx.local_with_repr("C".to_string(),
            Term::abstract_pi(vec![rcx.ty_cx.local_with_repr("".to_string(), ty)],
                Term::Type));

        rcx.ind_hyp = ind_hyp;

        rcx
    }

    fn eta_expand(&self) -> Term {
        let mut pi = self.inductive_ty.ty.clone();
        let mut result = self.inductive_ty.name.to_term();
        let mut i = 0;
        let mut locals = vec![];
        while let Term::Forall { ty, term, .. } = pi {
            let local = self.ty_cx.local_with_repr(format!("x{}", 0), *ty);
            locals.push(local);
            pi = *term;
        }
        Term::abstract_lambda(
            locals.clone(),
            Term::apply_all(result, locals.into_iter().map(|t| t.to_term()).collect()))
    }

    pub fn with_params(&self, term: Term) -> Term {
        let params = self.inductive_ty
                         .parameters
                         .iter()
                         .map(|x| x.to_term())
                         .collect();

        Term::apply_all(term, params).whnf()
    }

    fn is_recursive_arg(&self, term: &Term) -> bool {
        match term.head() {
            None => false,
            Some(h) => h == self.inductive_ty.name.to_term(),
        }
    }

    pub fn minor_premise_for(&mut self, ind_hyp: &Name, ctor: &(Name, Term)) -> Term {
        // Apply the constructor name to the parameters.
        let ctor_with_params =
            self.with_params(ctor.0.to_term());

        // Apply the constructor type to the parameters.
        let mut ctor_ty_with_params =
            self.with_params(ctor.1.clone());

        let mut i = 0;
        let mut binders = Vec::new();
        let mut arguments = Vec::new();
        let mut pi = &ctor_ty_with_params;

        while let &Term::Forall { ref ty, ref term, .. } = pi {
            // Create a local with a fresh name and type of the binder.
            let arg_local =
                self.ty_cx.local_with_repr(
                    format!("a{}", i),
                    *ty.clone());

            // Add this to the list of binders (a0 : A) (a1 : List A)
            binders.push(arg_local.clone());
            // (a0 : A) (a1 : List A) -> C a1 -> C (Cons a0 a1)

            // If this is a recursive argument we all need to generate a piece of proof
            // for that case for example `C a1`.
            if self.is_recursive_arg(&*ty) {
                let local_x =
                    self.ty_cx.local_with_repr(
                        "".to_string(),
                        Term::apply(ind_hyp.to_term(), arg_local.to_term()));

                 arguments.push(local_x);
            }

            pi = &**term;
            i += 1;
        }

        let c_for_ctor = Term::apply(
            ind_hyp.to_term(),
            Term::apply_all(
                ctor_with_params,
                binders.iter()
                       .map(|x| x.to_term())
                       .collect()));

        Term::abstract_pi(
            binders,
            Term::abstract_pi(
                arguments,
                c_for_ctor))
    }
}

/// Construct a recursor for `data_type`.
pub fn make_recursor(ty_cx: &mut TyCtxt, data_type: &Data) {
    let mut rcx = RecursorCx::new(ty_cx, data_type);

    let params = data_type.parameters.clone();

    let ind_hyp = rcx.ind_hyp.clone();

    let mut premises = Vec::new();

    for ctor in &data_type.ctors {
        premises.push(rcx.minor_premise_for(&ind_hyp, ctor))
    }

    let premises: Vec<_> = premises.into_iter()
                                   .map(|p| rcx.ty_cx.local_with_repr("".to_string(), p))
                                   .collect();

    let mut result = data_type.ty.clone();

    let mut tys = Vec::new();
    while let Term::Forall { ty, term, .. } = result {
        tys.push(*ty.clone());
        result = *term;
    }

    tys.push(data_type.name.to_term());

    let tys: Vec<_> = tys.into_iter()
                         .enumerate()
                         .map(|(i, ty)| rcx.ty_cx.local_with_repr(format!("a{}", i), ty))
                         .collect();

    let tys_terms: Vec<_> = tys.iter().map(|t| t.to_term()).collect();

    let recursor_ty =
        Term::abstract_pi(
            params.clone(),
            Term::abstract_pi(vec![ind_hyp.clone()],
                Term::abstract_pi(premises.clone(),
                    Term::abstract_pi(tys.clone(),
                        Term::apply_all(ind_hyp.to_term(),
                        tys_terms.clone())))));

    // println!("declare_datatype: recursor_ty={}", recursor_ty);

    let mut inner_terms = vec![ind_hyp.clone().to_term()];
    inner_terms.extend(premises.clone().into_iter().map(|x| x.to_term()));
    inner_terms.extend(tys_terms.clone().into_iter());

    let recursor_body =
        Term::abstract_lambda(
            params.clone(),
            Term::abstract_lambda(
                vec![ind_hyp.clone()],
                Term::abstract_lambda(
                    premises,
                    Term::abstract_lambda(
                        tys,
                        Term::Recursor(
                            data_type.name.clone(),
                            1,
                            inner_terms)))));

    rcx.ty_cx.definitions.insert(data_type.name.in_scope("rec".to_string()).unwrap(),
                            (recursor_ty, recursor_body));
}
