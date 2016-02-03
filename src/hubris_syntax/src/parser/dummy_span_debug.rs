use super::super::ast::*;
use super::super::visit::*;

pub fn ensure_no_dummy_spans(module: &Module) {
    let mut dsv = DummySpanVisitor { count: 0 };
    dsv.visit_module(module);
    assert_eq!(dsv.count, 0);
}

struct DummySpanVisitor {
    count: usize
}

impl<'v> Visitor<'v> for DummySpanVisitor {
    fn visit_span(&mut self, span: Span) {
        self.count += 1;
    }
}
