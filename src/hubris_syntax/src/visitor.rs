use super::ast::*;

pub trait Visitor {
    pub fn visit_module(&mut self, module: Module) {
        panic!(),
    }

    pub fn visit_item(&mut self, item: Item) {
        panic!()
    }

    pub fn visit_data(&mut self, data: Data) {
        panic!()
    }

    pub fn visit_extern(&mut self, ext: Extern) {
        panic!()
    }

    pub fn visit_def(&mut self, def: Function) {
        panic!()
    }

    pub fn visit_term(&mut self, term: Term) {
        panic!()
    }

    pub fn visit_span(&mut self, span: Span) {
        panic!()
    }

    pub fn visit_case(&mut self, case: Case) {
        panic!()
    }

    pub fn visit_pattern(&mut self, pattern: Pattern) {
        panic!()
    }

    pub fn visit_literal(&mut self, lit: Literal) {
        panic!()
    }
}
