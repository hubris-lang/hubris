use super::{TyCtxt, Error, LocalCx};

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

        let ty = rcx.make_ind_hyp_ty();

        let ty = rcx.with_params(ty);
        rcx.ind_hyp = rcx.ty_cx.local_with_repr("C".to_string(), ty);
        rcx
    }

    fn  make_ind_hyp_ty(&self) -> Term {
        let mut pi = self.inductive_ty.ty.clone();
        let mut result = self.inductive_ty.name.to_term();
        let mut i = 0;
        let mut locals = vec![];
        while let Term::Forall { ty, term, .. } = pi {
            let local = self.ty_cx.local_with_repr(format!("x{}", 0), *ty);
            locals.push(local);
            pi = *term;
        }

        let applied_ty =
            Term::apply_all(
                result,
                locals.iter().map(|t| t.to_term()).collect());

        locals.push(
            self.ty_cx.local_with_repr("".to_string(), applied_ty));

        Term::abstract_pi(
            locals.clone(),
            Term::Type)
    }

    pub fn with_params(&self, term: Term) -> Term {
        let params = self.inductive_ty
                         .parameters
                         .iter()
                         .map(|x| x.to_term())
                         .collect();

        self.ty_cx.eval(&Term::apply_all(term, params)).unwrap() // TODO: flow error
    }

    fn is_recursive_arg(&self, term: &Term) -> bool {
        match term.head() {
            None => false,
            Some(h) => h == self.inductive_ty.name.to_term(),
        }
    }

    pub fn minor_premise_for(&mut self, ind_hyp: &Name, ctor: &(Name, Term)) -> Result<Term, Error> {
        // Apply the constructor name to the parameters.
        let ctor_with_params =
            self.with_params(ctor.0.to_term());

        // Apply the constructor type to the parameters.
        let mut ctor_ty_with_params =
            self.with_params(ctor.1.clone());

        let mut i = 0;
        let mut binders = Vec::new();
        let mut arguments = Vec::new();
        let mut pi = ctor_ty_with_params;

        while let Term::Forall { ty, term, .. } = pi {
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
                let num_params = self.inductive_ty.parameters.len();
                let mut indicies = match ty.args() {
                    None => vec![],
                    Some(is) =>
                        is.iter()
                          .skip(num_params)
                          .map(Clone::clone)
                          .collect(),
                };

                // println!("type to get indicies from: {}", ty);

                // Add the ctor to the end of the list and we are going to build an
                // application of the form C indicies (Ctor args)
                indicies.push(arg_local.to_term());

                let local_x =
                    self.ty_cx.local_with_repr(
                        "".to_string(),
                        Term::apply_all(
                            ind_hyp.to_term(),
                            indicies));

                 arguments.push(local_x);
            }

            pi = term.instantiate(&arg_local.to_term());
            i += 1;
        }

        let ctor_application =
            Term::apply_all(
                ctor_with_params,
                binders.iter()
                       .map(|x| x.to_term())
                       .collect());

        // We compute the type of the constructor using the type checker
        // this allows us to rely on the type system to return the proper
        // set of indicies for us to bind.
        let ty_of_ctor = {
            let mut lcx = LocalCx::from_cx(self.ty_cx);
            try!(lcx.type_infer_term(&ctor_application))
        };

        // We then compute indicies in the same way as above.
        let num_params = self.inductive_ty.parameters.len();
        let mut indicies = match ty_of_ctor.args() {
            None => vec![],
            Some(is) =>
                is.iter()
                  .skip(num_params)
                  .map(Clone::clone)
                  .collect(),
        };

        indicies.push(ctor_application);

        let c_for_ctor = Term::apply_all(
            ind_hyp.to_term(),
            indicies);

        Ok(Term::abstract_pi(
              binders,
              Term::abstract_pi(
                  arguments,
                  c_for_ctor)))
    }

    pub fn construct_recursor(&self,
                              minor_premises: Vec<Name>,
                              major_premise_args: Vec<Name>,
                              major_premise: Term) -> (Term, Term) {
        let ind_hyp = self.ind_hyp.clone();
        let params =
            self.inductive_ty
                .parameters
                .clone();

        let recursor_ty =
            Term::abstract_pi(
                params.clone(),
                Term::abstract_pi(vec![ind_hyp.clone()],
                    Term::abstract_pi(minor_premises.clone(),
                        Term::abstract_pi(major_premise_args.clone(),
                            major_premise))));

        let mut recursor_terms = vec![ind_hyp.clone().to_term()];
        recursor_terms.extend(minor_premises.iter().map(|x| x.to_term()));
        recursor_terms.extend(major_premise_args.iter().map(|x| x.to_term()));

        let recursor_body =
            Term::abstract_lambda(
                params.clone(),
                Term::abstract_lambda(
                    vec![ind_hyp.clone()],
                    Term::abstract_lambda(
                        minor_premises,
                        Term::abstract_lambda(
                            major_premise_args,
                            Term::Recursor(
                                self.inductive_ty.name.clone(),
                                1, // this needs to be fixed
                                recursor_terms)))));

        (recursor_ty, recursor_body)
    }

    pub fn major_premise(&self) -> (Vec<Name>, Term) {
        let mut data_type_ty = self.with_params(
            self.inductive_ty.ty.clone());

        let mut arguments = Vec::new();
        while let Term::Forall { ty, term, .. } = data_type_ty {
            arguments.push(*ty.clone());
            data_type_ty = *term;
        }

        let mut arguments: Vec<_> = arguments.into_iter()
                                         .enumerate()
                                         .map(|(i, ty)|
                                            self.ty_cx.local_with_repr(format!("a{}", i), ty))
                                         .collect();
        let scrutinee =
            Term::apply_all(
                self.with_params(self.inductive_ty.name.to_term()),
                arguments.iter().map(|x| x.to_term()).collect());

        arguments.push(self.ty_cx.local_with_repr(
            "c".to_string(),
            scrutinee));

        let premise =
            self.ind_hyp.to_term();

        let premise = Term::apply_all(
            premise,
            arguments.
            iter().
            map(|a| a.to_term()).
            collect());

        (arguments, premise)
    }
}

/// Construct a recursor for `data_type`.
pub fn make_recursor(ty_cx: &mut TyCtxt, data_type: &Data) -> Result<(), Error> {
    let mut rcx = RecursorCx::new(ty_cx, data_type);

    let ind_hyp = rcx.ind_hyp.clone();

    let params = data_type.parameters.clone();

    let minor_premises: Result<_, Error> =
        data_type.ctors
                 .iter()
                 .map(|ctor| {
                     let p = try!(rcx.minor_premise_for(&ind_hyp, ctor));
                     Ok(rcx.ty_cx.local_with_repr("".to_string(), p))
                 })
                 .collect();

    let minor_premises = try!(minor_premises);

    let (tys, major_premise) =
        rcx.major_premise();

    let (recursor_ty, recursor_body) =
        rcx.construct_recursor(
            minor_premises,
            tys,
            major_premise);

    let recursor_name = data_type.name.in_scope("rec".to_string()).unwrap();

    rcx.ty_cx
       .definitions
       .insert(recursor_name, (recursor_ty, recursor_body));

    Ok(())
}
