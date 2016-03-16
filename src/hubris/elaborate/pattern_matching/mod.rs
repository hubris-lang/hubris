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


    fn elaborate_simple_match(&mut self, simple_match: SimpleMatch) -> Result<core::Term, Error> {
        let SimpleMatch {
            scrutinee,
            cases,
            pattern_type,
        } = simple_match;

        let escrutinee = try!(self.elab_cx.elaborate_term(scrutinee));

        let (inductive_ty, args) =
            try!(self.elab_cx.cx.ty_cx.type_check_term(&escrutinee, None)).1.uncurry();

        let inductive_ty = match inductive_ty {
            Term::Var { name } => name,
            _ => panic!()
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

        let cases : Vec<_> = try!(cases.into_iter().map(|c| self.elaborate_simple_case(c)).collect());

         for case in &cases {
            println!("core case: {}", case.1);
         }

         panic!()
    }

    fn elaborate_simple_case(&mut self, simple_case: SimpleCase) -> Result<(core::Name, core::Term), Error> {
        let SimpleCase {
            pattern,
            rhs,
        } = simple_case;

        panic!()
        // self.enter_pattern_scope(pattern
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
