use super::{TyCtxt, ComputationRule, Error};
use super::super::core::*;
//use super::name_generator::*;

/// `InductiveCx` packages the state needed state to process an inductive definition.
pub struct InductiveCx<'i, 'tcx> {
    ty_cx: &'tcx mut TyCtxt,
    inductive_ty: &'i Data,
    ind_hyp: Name,
}

/// `Recursor` describes the recursor for a inductive type, each field
/// is split here, enabling easy manipulation of recursors.
struct Recursor {
    motif: Name,
    parameters: Vec<Name>,
    minor_premises: Vec<Name>,
    major_premise: Term,
    ty: Term,
    name: Name,
    computation_rule: ComputationRule,
}

impl<'i, 'tcx> InductiveCx<'i, 'tcx> {
    ///
    fn new(ty_cx: &'tcx mut TyCtxt, inductive_ty: &'i Data) -> InductiveCx<'i, 'tcx> {

        let mut rcx = InductiveCx {
            ty_cx: ty_cx,
            inductive_ty: inductive_ty,
            ind_hyp: inductive_ty.name.clone(),
        };

        let ty = rcx.make_ind_hyp_ty();

        rcx.ind_hyp = rcx.ty_cx.local_with_repr("C".to_string(), ty);
        rcx
    }

    fn make_ind_hyp_ty(&self) -> Term {
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

        // debug!("ctor_ty_with_params: {}", ctor_ty_with_params);

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

                // debug!("type to get indicies from: {}", ty);

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

    pub fn construct_recursor_ty(&self,
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
            debug!("term {}", term);
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

            debug!("ty_name: {}", ty_name);
            let scrutinee = &args[args.len() - 1];
            let scrutinee = try!(cx.eval(scrutinee));
            debug!("scrutinee: {}", scrutinee);
            let (scrut_ctor, scrut_args) = scrutinee.uncurry();

            match cx.types.get(&ty_name) {
                None => panic!("type checking bug: can not find inductive type {}", ty_name),
                Some(dt) => {
                    let params = &dt.parameters;

                    let motif_and_premise : Vec<_> =
                        args.iter()
                            .skip(params.len())
                            .cloned()
                            .collect();

                    let motif = motif_and_premise[0].clone();

                    let premises : Vec<_> =
                        motif_and_premise.iter()
                                         .skip(1)
                                         .cloned()
                                         .collect();

                    for (i, ctor) in dt.ctors.iter().enumerate() {
                        let name = &ctor.0;
                        let ctor_ty = &ctor.1;

                        if scrut_ctor == name.to_term() {
                            let premise = premises[i].clone();

                            let is_recursive =
                                cx.is_recursive_ctor(&ty_name, ctor_ty);

                            if !is_recursive {
                                // Remember to remove the parameters, since
                                // the premise is not parametrized by them.
                                let args : Vec<_> =
                                    scrut_args.iter()
                                              .skip(dt.parameters.len())
                                              .cloned()
                                              .collect();

                                return cx.eval(&Term::apply_all(premise, args));
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
                            // debug!("premise: {}", premise);
                            // debug!("scurtinee: {}", scrutinee);
                            //
                            // let mut term_args = vec![];
                            // let mut recursor_args = vec![];
                            //
                            // for (arg, ty) in args.zip(tys.into_iter()) {
                            //     debug!("arg : {}", arg);
                            //     debug!("ty : {}", ty);
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
            }
            panic!("this shouldn't happen")
        }))
    }

    fn recursor(&mut self) -> Result<Recursor, Error> {
        let motif = self.ind_hyp.clone();

        let params =
            self.inductive_ty
                .parameters
                .clone();

        let minor_premises: Result<_, Error> =
            self.inductive_ty.ctors
                             .iter()
                             .map(|ctor| {
                                 let p = try!(self.minor_premise_for(&motif, ctor));
                                 Ok(self.ty_cx.local_with_repr("".to_string(), p))
                             })
                             .collect();

        let minor_premises: Vec<_> = try!(minor_premises);

        let (tys, major_premise) =
            self.major_premise();

        let recursor_ty =
            self.construct_recursor_ty(
                minor_premises.clone(),
                tys,
                major_premise.clone());

        let recursor_name =
            self.inductive_ty.name.in_scope("rec".to_string()).unwrap();

        let computation_rule = try!(self.construct_computation_rule());

        Ok(Recursor {
            motif: motif,
            parameters: params,
            minor_premises: minor_premises,
            major_premise: major_premise,
            ty: recursor_ty,
            name: recursor_name,
            computation_rule: computation_rule,
        })
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

        debug!("{}", def);

        try!(self.ty_cx.declare_def(&def));

        Ok(())
    }

    pub fn make_cases_on(&mut self) -> Result<(), Error> {
            let name = self.inductive_ty
                           .name
                           .in_scope("cases_on".to_string())
                           .unwrap();

            let params : Vec<_> = self.inductive_ty
                             .parameters
                             .clone();

            let params_as_terms : Vec<_> =
                params.clone()
                      .iter()
                      .map(|p| p.to_term())
                      .collect();

            let inductive_ty = self.inductive_ty;
            let ind_hyp = self.ind_hyp.clone();

            let minor_premises: Result<_, Error> =
                inductive_ty.ctors
                   .iter()
                   .map(|ctor| {
                       let p = try!(self.minor_premise_for(&ind_hyp , ctor));
                       debug!("{}", p);
                       Ok(self.ty_cx.local_with_repr("".to_string(), p))
                   })
                   .collect();

            let minor_premises : Vec<Name> = try!(minor_premises);

            let scrut =
                self.ty_cx.local_with_repr(
                    "n".to_string(),
                    Term::apply_all(
                        self.inductive_ty.name.to_term(),
                       params_as_terms.clone()));

            let ty =
                Term::abstract_pi_implicit(
                    params.clone(),
                    Term::abstract_pi_implicit(
                        vec![self.ind_hyp.clone()],
                        Term::abstract_pi(
                            vec![scrut.clone()],
                            Term::abstract_pi(
                                minor_premises.clone(),
                                Term::apply_all(
                                    self.ind_hyp.to_term(),
                                    vec![scrut.to_term()])))));

            let rec =
                self.inductive_ty
                    .name
                    .in_scope("rec".to_string())
                    .unwrap();

            let mut rec_args = vec![];
            for premise in minor_premises.clone() {
                let ty = try!(self.ty_cx.type_infer_term(&premise.to_term()));
                let mut ty = ty.0;
                // println!("premise={} ty={}", premise, ty);
                let mut locals = vec![];
                while let Term::Forall { binder, term, .. } = ty {
                    locals.push(self.ty_cx.local(binder).with_repr("f".to_string()));
                    ty = *term;
                }

                for local in locals.iter().rev() {
                    // println!("{}", local);
                }
                // If this argument is recursive we should skip it
                // println!("transformed: {}", Term::apply_all(premise.to_term(), vec![]));
                rec_args.push(premise.with_repr("f".to_string()));
            }

            let mut recursor_args = params_as_terms.clone();
            recursor_args.push(self.ind_hyp.to_term());
            recursor_args.extend(rec_args.clone().into_iter().map(|x| x.to_term()));
            recursor_args.push(scrut.to_term());

            let body =
                Term::abstract_lambda(
                    params.clone(),
                    Term::abstract_lambda(
                        vec![self.ind_hyp.clone(), scrut],
                        Term::abstract_lambda(
                            rec_args.clone(),
                            Term::apply_all(
                                rec.to_term(),
                                recursor_args))));

            let def = Function {
                name: name,
                args: vec![],
                ret_ty: ty,
                body: body,
            };

            // println!("{}", def);
            self.ty_cx.declare_def(&def)
        }
}

/// Construct a recursor for `data_type`.
pub fn make_recursor(ty_cx: &mut TyCtxt, data_type: &Data) -> Result<(), Error> {
    let mut rcx = InductiveCx::new(ty_cx, data_type);
    let recursor = try!(rcx.recursor());

    // Add an axiom with the recursor type, and the associated computation rule.
    rcx.ty_cx
       .axioms
       .insert(recursor.name, super::Axiom {
           ty: recursor.ty,
           computation_rule: Some(recursor.computation_rule),
       });

    // Now setup all the automatically generated constructs.
    try!(rcx.make_cases_on());

    Ok(())
}
