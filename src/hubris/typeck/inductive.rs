use super::{TyCtxt, ComputationRule, Error};
use super::super::core::*;
//use super::name_generator::*;

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

        rcx.ind_hyp = rcx.ty_cx.local_with_repr("C".to_string(), ty);
        rcx
    }

    fn  make_ind_hyp_ty(&self) -> Term {
        let mut pi =
            self.with_params(self.inductive_ty.ty.clone());
        let  result =
            self.with_params(self.inductive_ty.name.to_term());

        let mut i = 0;
        let mut locals = vec![];

        while let Term::Forall { binder, term, .. } = pi {
            let local =
                self.ty_cx
                    .local_with_repr(
                        format!("x{}", i),
                        *binder.ty);

            locals.push(local);
            pi = *term;
            i += 1;
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

    // A helper for applying parameters to different types of terms.
    pub fn with_params(&self, term: Term) -> Term {
        let params: Vec<_> = self.inductive_ty
                                 .parameters
                                 .iter()
                                 .map(|x| x.to_term())
                                 .collect();

        // If there are no parameters we shouldn't try to apply
        // them.
        if params.len() == 0 {
            return term;
        }

        // If it is a forall we should instantiate it
        if let &Term::Forall { .. } = &term {
            let mut term = term;
            for param in params {
                term = match term {
                    Term::Forall { term: term1, .. } =>
                    term1.instantiate(&param),
                    t => panic!("{}", t)
                }
            }

            return term;
        // If it is a var we apply it
        } else if let &Term::Var { .. } = &term {
            return Term::apply_all(term, params)
        } else {
            panic!("{}", term)
        }
    }

    fn is_recursive_arg(&self, term: &Term) -> bool {
        debug!("is_recursive_arg: term={}", term);
        match term.head() {
            None => false,
            Some(h) => h == self.inductive_ty.name.to_term(),
        }
    }

    pub fn minor_premise_for(&mut self, ind_hyp: &Name, ctor: &(Name, Term)) -> Result<Term, Error> {
        debug!("minor_premise_for: ind_hyp={} ctor=({}, {})", ind_hyp, ctor.0, ctor.1);
        // Apply the constructor name to the parameters.
        let ctor_with_params =
            self.with_params(ctor.0.to_term());

        // Apply the constructor type to the parameters.
        let ctor_ty_with_params =
            self.with_params(ctor.1.clone());

        // println!("ctor_ty_with_params: {}", ctor_ty_with_params);

        let mut i = 0;
        let mut binders = Vec::new();
        let mut arguments = Vec::new();
        let mut pi = ctor_ty_with_params;

        while let Term::Forall { binder, term, .. } = pi {
            let name = binder.name;
            let ty = binder.ty;

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
        let (ty_of_ctor, cs) =
            try!(self.ty_cx.type_infer_term(&ctor_application));

        // We should assert that this resulting term contains no meta variables

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
                              major_premise: Term) -> Term {
        let ind_hyp = self.ind_hyp.clone();
        let params =
            self.inductive_ty
                .parameters
                .clone();

        let recursor_ty =
            Term::abstract_pi_implicit(
                params.clone(),
                Term::abstract_pi(vec![ind_hyp.clone()],
                    Term::abstract_pi(minor_premises.clone(),
                        Term::abstract_pi(major_premise_args.clone(),
                            major_premise))));

        let recursor_terms: Vec<_> =
            minor_premises
                .iter()
                .map(|x| x.to_term())
                .collect();


        let scrutinee =
            major_premise_args.iter()
                              .last()
                              .unwrap()
                              .to_term();

        recursor_ty
    }

    pub fn major_premise(&self) -> (Vec<Name>, Term) {
        let mut data_type_ty = self.with_params(
            self.inductive_ty.ty.clone());

        let mut arguments = Vec::new();
        while let Term::Forall { binder, term,.. } = data_type_ty {
            arguments.push(*binder.ty);
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


    pub fn construct_computation_rule(&self) -> Result<ComputationRule, Error> {
        Ok(Box::new(|cx: &TyCtxt, term: Term| {
            // BUG WARNING: this code IS NOT general enough
            println!("term {}", term);
            let (head, args) = term.uncurry();
            let ty_name = match &head {
                &Term::Var { ref name } => match name {
                    &Name::Qual { ref components, span } => Name::Qual {
                        components: vec![components[0].clone()],
                        span: span,
                    },
                    _ => panic!()
                },
                _ => panic!()
            };

            println!("ty_name: {}", ty_name);
            let scrutinee = &args[args.len() - 1];
            let scrutinee = try!(cx.eval(scrutinee));
            println!("scrutinee: {}", scrutinee);
            let (scrut_ctor, scrut_args) = scrutinee.uncurry();


            match cx.types.get(&ty_name) {
                None => panic!("type checking bug: can not find inductive type {}", ty_name),
                Some(dt) => {
                    let params = &dt.parameters;

                    let premises : Vec<_> =
                        args.iter()
                            .skip(params.len())
                            .cloned()
                            .collect();

                    for (i, ctor) in dt.ctors.iter().enumerate() {
                        let name = &ctor.0;
                        let ctor_ty = &ctor.1;

                        if scrut_ctor == name.to_term() {
                            let premise = premises[i].clone();
                            panic!("{}", premise);
                            //
                            // let is_recursive =
                            //     self.is_recursive_ctor(ty_name, ctor_ty);
                            //
                            // if !is_recursive {
                            //     let args: Vec<_> =
                            //         scrutinee.args()
                            //                  .unwrap();
                            //
                            // // Need to skip the parameters
                            // let args =
                            //     args.iter()
                            //         .skip(dt.parameters.len())
                            //         .cloned()
                            //         .collect();
                            //
                            // return self.eval(&Term::apply_all(premise, args));
                        } else {
                            panic!()
                            // let args: Vec<_> =
                            //     scrutinee.args()
                            //              .unwrap();
                            //
                            // // Need to skip the parameters
                            // let args =
                            //     args.iter()
                            //         .skip(dt.parameters.len());
                            //
                            // let tys =
                            //     premise.binders()
                            //            .unwrap();
                            //
                            // println!("premise: {}", premise);
                            // println!("scurtinee: {}", scrutinee);
                            //
                            // let mut term_args = vec![];
                            // let mut recursor_args = vec![];
                            //
                            // for (arg, ty) in args.zip(tys.into_iter()) {
                            //     println!("arg : {}", arg);
                            //     println!("ty : {}", ty);
                            //     if ty.head().unwrap() == ty_name.to_term() {
                            //         let rec =
                            //         Recursor(
                            //             ty_name.clone(),
                            //             premises.clone(),
                            //             Box::new(arg.clone()));
                            //             recursor_args.push(rec);
                            //     }
                            //
                            //     term_args.push(arg.clone());
                            // }
                            //
                            // let mut args = term_args;
                            // args.extend(recursor_args.into_iter());
                            //
                            // return self.eval(&Term::apply_all(premise.clone(), args));
                        }
                    }
                }
            }
            panic!("this shouldn't happen")
        }))
    }

    pub fn make_below(&mut self) -> Result<(), Error> {
        let name = self.inductive_ty
                       .name
                       .in_scope("below".to_string())
                       .unwrap();

        let params : Vec<_> = self.inductive_ty
                         .parameters
                         .clone();

        let params_as_terms : Vec<_> =
            params.clone()
                  .iter()
                  .map(|p| p.to_term())
                  .collect();

        let ty =
            Term::abstract_pi_implicit(
                params.clone(),
                Term::apply_all(
                    self.inductive_ty.name.to_term(),
                    params_as_terms.clone()));

        let rec =
            self.inductive_ty
                .name
                .in_scope("rec".to_string())
                .unwrap();

        let body =
            Term::abstract_lambda(
                params.clone(),
                Term::apply_all(
                    rec.to_term(),
                    params_as_terms));

        let def = Function {
            name: name,
            args: vec![],
            ret_ty: ty,
            body: body,
        };

        println!("{}", def);

        // try!(self.ty_cx.declare_def(&def));

        Ok(())
        // def below {C : Nat -> Type} (n : Nat) : Type :=
        //     Nat.rec
        //     _
        //     Star
        //     (fun (m : Nat) (proof : Type) => MkProd (C m) proof)
        //     n
        //  end


        // nat.below [reducible] [unfold 1] : Π {C : ℕ → Type}, ℕ → Type
        // λ {C : ℕ → Type} (n : ℕ), nat.rec poly_unit (λ (a : ℕ) (v_0 : Type), C a × v_0) n
    }
}

/// Construct a recursor for `data_type`.
pub fn make_recursor(ty_cx: &mut TyCtxt, data_type: &Data) -> Result<(), Error> {
    let mut rcx = RecursorCx::new(ty_cx, data_type);

    let ind_hyp = rcx.ind_hyp.clone();

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

    let recursor_ty =
        rcx.construct_recursor(
            minor_premises,
            tys,
            major_premise);

    let recursor_name = data_type.name.in_scope("rec".to_string()).unwrap();

    let computation_rule = try!(rcx.construct_computation_rule());

    // Construct the recursor.
    rcx.ty_cx
       .axioms
       .insert(recursor_name, super::Axiom {
           ty: recursor_ty,
           computation_rule: Some(computation_rule),
       });

    // Now setup all the automatically generated constructs.
    try!(rcx.make_below());

    Ok(())
}
