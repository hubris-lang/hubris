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


        let params = inductive_ty.parameters.iter().map(|x| x.to_term()).collect();
        let ty = Term::apply_all(ty, params).whnf();
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

    pub fn make_premise(&mut self, ind_hyp: &Name, ctor: &(Name, Term)) -> Term {
        let ctor_name = &ctor.0;
        let mut ctor_ty = &ctor.1;

        let mut premise_args = Vec::new();
        let mut names = Vec::new();

        let mut i = 0;
        while let &Term::Forall { ref ty, ref term, .. } = ctor_ty {
            let local_n = self.ty_cx.local_with_repr(format!("a{}", i), *ty.clone());

            premise_args.push(local_n.clone());

            let local_x =
                self.ty_cx.local_with_repr(
                    "".to_string(),
                    Term::apply(ind_hyp.to_term(), local_n.to_term()));

            premise_args.push(local_x);

            if true {
                names.push(local_n.to_term());
            }

            ctor_ty = &**term;
            i += 1;
        }

        let c_for_ctor = Term::apply(
            ind_hyp.to_term(),
            Term::apply_all(ctor_name.to_term(), names));

        Term::abstract_pi(premise_args, c_for_ctor)
    }
}

/// Construct a recursor for `data_type`.
pub fn make_recursor(ty_cx: &mut TyCtxt, data_type: &Data) {
    let mut rcx = RecursorCx::new(ty_cx, data_type);

    let params = data_type.parameters.clone();

    let ind_hyp = rcx.ind_hyp.clone();

    let mut premises = Vec::new();

    for ctor in &data_type.ctors {
        premises.push(rcx.make_premise(&ind_hyp, ctor))
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
