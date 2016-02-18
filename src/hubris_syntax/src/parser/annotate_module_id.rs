use super::super::ast::{ModuleId, Module, Span};
use super::super::visit::*;

pub struct ModuleIdAnnotator {
    pub id: ModuleId,
}

impl<'v> VisitorMut<'v> for ModuleIdAnnotator {
    fn visit_mut_span(&mut self, span: &'v mut Span) {
        span.module_id = self.id;
    }
}

pub fn annotate_module_id(module: &mut Module, id: ModuleId) {
    let mut annotator = ModuleIdAnnotator {
        id: id
    };

    annotator.visit_mut_module(module)
}
