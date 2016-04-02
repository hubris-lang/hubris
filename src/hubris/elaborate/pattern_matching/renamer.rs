use super::super::super::ast::{self, Term};
use super::super::super::syntax::visit::*;

use std::collections::HashMap;

pub type RenameMap = HashMap<ast::Name, ast::Term>;

struct Renamer {
    rename_map: RenameMap,
}

impl<'v> VisitorMut<'v> for Renamer {
    fn visit_mut_term(&mut self, term: &'v mut Term) {
        let replace = match term {
            &mut Term::Var { ref name, .. } => {
                match self.rename_map.get(&name) {
                    None => None,
                    Some(t) => Some(t.clone())
                }
            }
            _ => None,
        };

        match replace {
            None => {}
            Some(t) => *term = t,
        }

        walk_mut_term(self, term)
    }
}

pub fn rename_term(rename_map: RenameMap, term: &mut ast::Term) {
    let mut renamer = Renamer {
        rename_map: rename_map,
    };

    renamer.visit_mut_term(term);
}
