use super::super::ast::{self, SourceMap, HasSpan};
use super::super::core;
use super::{LocalElabCx, Error};

// struct PatternMatchCx {
//     thing: ()
// }

pub fn elaborate_pattern_match<'ecx>(
        elab_cx: &mut LocalElabCx<'ecx>,
        scrutinee: ast::Term,
        cases: Vec<ast::Case>) -> Result<core::Term, Error> {
    panic!("match elaboration is currently disabled");
}
