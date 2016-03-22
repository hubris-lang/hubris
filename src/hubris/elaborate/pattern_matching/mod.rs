use super::super::ast::{self};
use super::super::core::{self, Term};
use super::{LocalElabCx, Error};

use std::collections::HashMap;

struct PatternMatchCx<'ecx, 'cx: 'ecx> {
    elab_cx: &'ecx mut LocalElabCx<'cx>,
}

mod renamer;
mod simplify;

use self::simplify::*;

impl<'ecx, 'cx: 'ecx>PatternMatchCx<'ecx, 'cx> {
    fn new(elab_cx: &'ecx mut LocalElabCx<'cx>) -> PatternMatchCx<'ecx, 'cx> {
        PatternMatchCx {
            elab_cx: elab_cx,
        }
    }

    #[inline]
    fn enter_pattern_scope<F, R>(&mut self,
                                 name_and_type: Vec<(ast::Name, core::Term)>,
                                 body: F)
                                 -> Result<R, Error>
        where F: FnOnce(&mut PatternMatchCx, Vec<core::Name>) -> Result<R, Error>
    {
        let mut locals = vec![];

        let old_context = self.elab_cx.locals.clone();
        let old_locals_in_order = self.elab_cx.locals_in_order.clone();

        // A binder can contain multiple names like so:
        // (A B C : T) will result in a binder with
        // 3 names to bind, so we then do an inner
        // loop.

        for (name, ty) in name_and_type {
            let repr = match name.clone().repr {
                ast::NameKind::Qualified(..) => panic!(),
                ast::NameKind::Unqualified(s) => s,
                ast::NameKind::Placeholder => "_".to_string(),
            };

            let local =
                self.elab_cx.cx.ty_cx.local_with_repr_and_mode(repr, ty, core::BindingMode::Explicit);

            self.elab_cx.locals.insert(name, local.clone());
            self.elab_cx.locals_in_order.push(local.clone());
            locals.push(local);
        }

        let result = try!(body(self, locals));

        // Restore the previous context.
        self.elab_cx.locals = old_context;
        self.elab_cx.locals_in_order = old_locals_in_order;

        Ok(result)
    }


    fn elaborate_simple_match(&mut self, simple_match: SimpleMatch) -> Result<core::Term, Error> {
        let SimpleMatch {
            scrutinee,
            cases,
            pattern_type,
        } = simple_match;

        let escrutinee = try!(self.elab_cx.elaborate_term(scrutinee));

        let scrutinee_ty =
            try!(self.elab_cx.cx.ty_cx.type_check_term(&escrutinee, None)).1;

        let (inductive_ty, args) = scrutinee_ty.uncurry();

        let inductive_ty = match inductive_ty {
            Term::Var { name } => name,
            other => panic!("{}", other),
        };

        let datatype = match self.elab_cx.cx.ty_cx.types.get(&inductive_ty) {
            None => panic!("can't fine dt decl"),
            Some(dt) => dt.clone(),
        };

        let ctor_map : HashMap<_, _> =
            datatype.ctors
                    .clone()
                    .into_iter()
                    .collect();

        let cases : Vec<_> =
            try!(cases.into_iter()
                      .map(|c| self.elaborate_simple_case(c, &scrutinee_ty, &ctor_map))
                      .collect());

         for case in &cases {
            println!("core case: {}", case);
         }

         match pattern_type  {
             PatternType::Cases => {
                let cases_on = inductive_ty.in_scope("cases_on".to_string()).unwrap();
                Ok(Term::apply_all(cases_on.to_term(), cases))
             }
         }
    }

    fn simple_pattern_binders(&mut self,
                              simple_pattern: SimplePattern,
                              scrutinee_ty: &core::Term,
                              ctor_map: &HashMap<core::Name, core::Term>) -> Result<Vec<(ast::Name, core::Term)>, Error> {
        match simple_pattern {
            SimplePattern::Name(n) => {
                let elab_name = try!(self.elab_cx.cx.elaborate_global_name(n.clone()));

                match ctor_map.get(&elab_name) {
                    None => return Ok(vec![(n, scrutinee_ty.clone())]),
                    Some(ctor_ty) => {
                        // Need to do error checking here
                        return Ok(vec![]);
                    }
                }
            }
            SimplePattern::Constructor(ctor, args) => {
                let elab_name = try!(self.elab_cx.cx.elaborate_global_name(ctor.clone()));

                match ctor_map.get(&elab_name) {
                    None => return Ok(vec![(ctor.clone(), scrutinee_ty.clone())]),
                    Some(ctor_ty) => {
                        println!("{:?}", ctor_ty.binders());
                        let binders =ctor_ty.binders()
                                            .unwrap_or(vec![])
                                            .iter()
                                            .skip(2) // Need to thread through params
                                            .cloned()
                                            .zip(args.into_iter())
                                            .map(|(t, n)| {
                                                (n, t.clone())
                                            }).collect();

                        return Ok(binders);
                    }
                }
            }
        }
    }

    fn elaborate_simple_case(&mut self,
                             simple_case: SimpleCase,
                             scrutinee_ty: &core::Term,
                             ctor_map: &HashMap<core::Name, core::Term>) -> Result<core::Term, Error> {
        let SimpleCase {
            pattern,
            rhs,
        } = simple_case;

        println!("pattern: {} rhs: {}", pattern, rhs);

        let binders = try!(self.simple_pattern_binders(
            pattern,
            scrutinee_ty,
            ctor_map));

        for &(ref n, ref ty) in &binders {
            println!("{} {}", n, ty);
        }

        self.enter_pattern_scope(binders, move |pat_cx, names| {
            match rhs {
                SimpleMatchArm::Term(rhs) =>
                    Ok(Term::abstract_lambda(names, try!(pat_cx.elab_cx.elaborate_term(rhs)))),
                SimpleMatchArm::Match(mat) =>
                    pat_cx.elaborate_simple_match(mat)
            }
        })
    }
}

pub fn elaborate_pattern_match<'ecx>(
        elab_cx: &mut LocalElabCx<'ecx>,
        scrutinee: ast::Term,
        cases: Vec<ast::Case>) -> Result<Term, Error> {
    let mut pmcx = PatternMatchCx::new(elab_cx);
    let simplified_match = simplify_match(scrutinee, cases);
    println!("simplified_match: {}", simplified_match);
    pmcx.elaborate_simple_match(simplified_match)
}
