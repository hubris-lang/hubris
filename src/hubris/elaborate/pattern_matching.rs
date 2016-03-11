use super::super::ast::{self, Pattern}; // SourceMap, HasSpan};
use super::super::core::Term;
use super::{LocalElabCx, Error};

use std::collections::HashMap;

// enum PatternType {
//     Cases,
// }
//
// // A struct representing a simple pattern match, i.e
// // one that can not have nested patterns.
// struct SimpleMatch {
//     scrutinee: core::Term,
//     cases: SimpleCase,
//     pattern_type: PatternType,
// }
//
// struct PatternMatchCx<'ecx> {
//     elab_cx: &mut LocalElabCx<'ecx>,
//
// }
//
// impl<'ecx> PatternMatchCx<'ecx> {
//     fn new(elab_cx: &mut LocalElabCx<'ecx>) -> PatternMatchCx<'ecx> {
//         PatternMatchCx {
//             elab_cx: elab_cx,
//         }
//     }
//
//     fn simplify_match()
// }

pub fn elaborate_pattern_match<'ecx>(
        elab_cx: &mut LocalElabCx<'ecx>,
        scrutinee: ast::Term,
        cases: Vec<ast::Case>) -> Result<Term, Error> {
    let escrutinee = try!(elab_cx.elaborate_term(scrutinee));
    let (inductive_ty, args) = try!(elab_cx.cx.ty_cx.type_infer_term(&escrutinee)).0.uncurry();

    let inductive_ty = match inductive_ty {
        Term::Var { name } => name,
        _ => panic!()
    };

    let datatype = match elab_cx.cx.ty_cx.types.get(&inductive_ty) {
        None => panic!("can't fine dt decl"),
        Some(dt) => dt.clone(),
    };

    let ctor_map : HashMap<_, _> =
        datatype.ctors
                .clone()
                .into_iter()
                .collect();
    // println!("{} : {}", escrutinee, ty.0.head().unwrap());
    // we need to decompse cases into a chunk of information
    // and we need to classify the match type
    for case in cases {
        match case.pattern {
            Pattern::Constructor(ctor, args) => {
                let elab_ctor = try!(elab_cx.elaborate_name(ctor));
                let elab_ctor = match elab_ctor {
                    Term::Var { name } => name,
                    _ => panic!(),
                };

                match ctor_map.get(&elab_ctor) {
                    None => panic!(),
                    Some(ctor_ty) => println!("{}", ctor_ty),
                }
            },
            _ => panic!(),
        }

        let rhs = case.rhs;
        // println!("{:?} {:?}", pattern, rhs);
        panic!("{:?}", rhs);
    }
    panic!()
}
