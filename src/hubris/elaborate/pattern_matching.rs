use super::super::ast::{self}; // SourceMap, HasSpan};
use super::super::core;
use super::{LocalElabCx, Error};

// struct PatternMatchCx {
//     thing: ()
// }

pub fn elaborate_pattern_match<'ecx>(
        elab_cx: &mut LocalElabCx<'ecx>,
        scrutinee: ast::Term,
        cases: Vec<ast::Case>) -> Result<core::Term, Error> {
            let escrutinee = try!(elab_cx.elaborate_term(scrutinee));
            let ty = try!(elab_cx.cx.ty_cx.type_infer_term(&escrutinee));
            println!("{} : {}", escrutinee, ty.0);

            for case in cases {
                let pattern = case.pattern;
                let rhs = case.rhs;
                println!("{:?} {:?}", pattern, rhs);
            }

            panic!()
}
