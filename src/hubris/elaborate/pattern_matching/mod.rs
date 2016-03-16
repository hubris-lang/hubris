use super::super::ast::{self};
use super::super::core::{self, Term};
use super::{LocalElabCx, Error};
use super::super::itertools::Itertools;

use std::collections::HashMap;

struct PatternMatchCx<'ecx, 'cx: 'ecx> {
    elab_cx: &'ecx mut LocalElabCx<'cx>,
}

mod simplify;

use self::simplify::*;

impl<'ecx, 'cx: 'ecx>PatternMatchCx<'ecx, 'cx> {
    fn new(elab_cx: &'ecx mut LocalElabCx<'cx>) -> PatternMatchCx<'ecx, 'cx> {
        PatternMatchCx {
            elab_cx: elab_cx,
        }
    }

    // fn simplify_match(&mut self,
    //                   scrutinee: ast::Term,
    //                   cases: Vec<ast::Case>) -> Result<SimpleMatch, Error> {
    //     let escrutinee =
    //         try!(self.elab_cx.elaborate_term(scrutinee));
    //
    //     let (inductive_ty, args) =
    //         try!(self.elab_cx.cx.ty_cx.type_check_term(&escrutinee, None)).1.uncurry();
    //
    //     let inductive_ty = match inductive_ty {
    //         Term::Var { name } => name,
    //         _ => panic!()
    //     };
    //
    //     let datatype = match self.elab_cx.cx.ty_cx.types.get(&inductive_ty) {
    //         None => panic!("can't fine dt decl"),
    //         Some(dt) => dt.clone(),
    //     };
    //
    //     let ctor_map : HashMap<_, _> =
    //         datatype.ctors
    //                 .clone()
    //                 .into_iter()
    //                 .collect();
    //
    //     let simple_cases = try!(self.simplify_cases(ctor_map, cases));
    //
    //     Ok(SimpleMatch {
    //         scrutinee: escrutinee,
    //         cases: simple_cases,
    //         pattern_type: PatternType::Cases,
    //     })
    // }
    //
    // fn simplify_cases(&mut self,
    //                   mut ctor_map: HashMap<core::Name, core::Term>,
    //                   cases: Vec<ast::Case>) -> Result<Vec<Term>, Error> {
    //     for (key, group) in cases.into_iter().group_by(|c| unapply(&c.pattern).0) {
    //         println!("key {}", key);
    //         println!("group {:?}", group);
    //         let ector = match try!(self.elab_cx.elaborate_name(key)) {
    //             core::Term::Var { name } => name,
    //             _ => panic!()
    //         };
    //         let ty = ctor_map.remove(&ector).unwrap();
    //         println!("{} : {}", ector, ty);
    //         if group.len() > 1 {
    //             for case in group {
    //                 println!("case {:?}", case);
    //             }
    //             //let mut names = vec![];
    //             //self.simplify_patterns(ty, ector, &mut names, &group[..], case.rh
    //         } else {
    //             ;
    //         }
    //     }
    //
    //     if ctor_map.len() != 0 {
    //         panic!("non-exhaustive patterns? not true, we really need to see whether the type
    //          of the scrutinee can be unified with the return type of ")
    //     }
    //
    //     panic!()
    // }
    //
    // // C p_1 .. p_n => rhs
    // //
    // fn simplify_patterns(
    //     &mut self,
    //     ctor_ty: &core::Term,
    //     outer_ctor: core::Name,
    //     names: &mut Vec<ast::Name>,
    //     input_patterns: &[ast::Pattern],
    //     rhs: ast::Term) -> Result<Term, Error> {
    //     // No more patterns to process
    //     if input_patterns.len() == 0 {
    //         panic!("at the end of the patterns")
    //     } else {
    //         // We are going to go backwards across the pattern building up
    //         // all the nested matches we need.
    //         for (key, group) in input_patterns.into_iter().group_by(|p| unapply(p).0) {
    //             println!("key {}", key);
    //             println!("group {:?}", group);
    //             if group.len() > 1 {
    //                 panic!()
    //             } else {
    //                 names.push(key);
    //             }
    //         }
    //         panic!()
    //     }
    // }

    //         // We need to get at the constructor map, and give each name a type
    //         // here.
    //
    //     let binder_tys = ctor_ty.binders().unwrap();
    //
    //     assert_eq!(binder_tys.len(), names.len());
    //
    //     // | MkProd a b => t
    //     // fun (a : A) (b : A) => t
    //     // match p with
    //     // | MkProd a b => t
    //     // Prod.rec
    //     let binders =
    //         names.into_iter()
    //              .zip(binder_tys.into_iter())
    //              .collect();
    //
    //     self.enter_pattern_scope(binder_tys, |elab_cx, names| {
    //         Ok(SimpleCase {
    //             term: Term::abstract_lambda(names, rhs)
    //         })
    //     })
    // }
    //
    // fn simplify_case(&mut self,
    //                 ctor_map: &HashMap<core::Name, core::Term>,
    //                 case: ast::Case) -> Result<Term, Error> {
    //     match case.pattern {
    //         ast::Pattern::Constructor(ctor, args) => {
    //             let elab_ctor = try!(self.elab_cx.elaborate_name(ctor));
    //             let elab_ctor = match elab_ctor {
    //                 Term::Var { name } => name,
    //                 _ => panic!(),
    //             };
    //
    //             let ctor_ty = match ctor_map.get(&elab_ctor) {
    //                 None => panic!(),
    //                 Some(ctor_ty) => ctor_ty,
    //             };
    //
    //             let mut names = vec![];
    //
    //             self.simplify_patterns(
    //                 ctor_ty,
    //                 elab_ctor,
    //                 &mut names,
    //                 &args[..],
    //                 case.rhs)
    //         },
    //         _ => panic!(),
    //     }
    // }

    fn enter_pattern_scope<F, R>(&mut self,
                                 name_and_type: Vec<(ast::Name, core::Term)>,
                                 body: F)
                                 -> Result<R, Error>
        where F: FnOnce(&mut LocalElabCx, Vec<core::Name>) -> Result<R, Error>
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

        let result = try!(body(&mut self.elab_cx, locals));

        // Restore the previous context.
        self.elab_cx.locals = old_context;
        self.elab_cx.locals_in_order = old_locals_in_order;

        Ok(result)
    }
}

// pub fn elaborate_top_level_pattern_match<'ecx>(
//         elab_cx: &mut LocalElabCx<'ecx>,
//         scrutinee: Vec<ast::Term>
//         cases: Vec<Vec<ast::Case>>) -> Result<Term, Error> {
//     let escrutinee = try!(elab_cx.elaborate_term(scrutinee));

pub fn elaborate_pattern_match<'ecx>(
        elab_cx: &mut LocalElabCx<'ecx>,
        scrutinee: ast::Term,
        cases: Vec<ast::Case>) -> Result<Term, Error> {
    let mut pmcx = PatternMatchCx::new(elab_cx);
    let simplified_match = simplify_match(scrutinee, cases);
    println!("simplified_match: {}", simplified_match);
    panic!()
}
