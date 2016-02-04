use super::core::*;

pub trait Visitor<'v> : Sized {
    fn visit_module(&mut self, module: &'v Module) {
        walk_module(self, module)
    }

    fn visit_item(&mut self, item: &'v Item) {
        walk_item(self, item)
    }

    fn visit_data(&mut self, inductive: &'v Data) {
        walk_inductive(self, inductive)
    }

    fn visit_extern(&mut self, ext: &'v Extern) {
        panic!();
    }

    fn visit_def(&mut self, def: &'v Function) {
        walk_def(self, def)
    }

    fn visit_term(&mut self, term: &'v Term) {
        walk_term(self, term)
    }

    fn visit_span(&mut self, _span: Span) {
        panic!();
    }

    fn visit_case(&mut self, case: &'v Case) {
        panic!();
    }

    fn visit_pattern(&mut self, pattern: &'v Pattern) {
        panic!();
    }

    fn visit_literal(&mut self, lit: &'v Literal) {
        panic!();
    }

    fn visit_name(&mut self, name: &'v Name) {
        walk_name(self, name)
    }
}

fn walk_module<'v, V: Visitor<'v>>(visitor: &mut V, module: &'v Module) {
    visitor.visit_span(module.span);
    visitor.visit_name(&module.name);

    for item in &module.items {
        visitor.visit_item(item);
    }
}

fn walk_item<'v, V: Visitor<'v>>(visitor: &mut V, item: &'v Item) {
    use ast::Item::*;

    match item {
        &Item::Data(ref d) => visitor.visit_data(d),
        &Item::Fn(ref d) => visitor.visit_def(d),
        &Item::Extern(ref ext) => panic!(),
        &Item::Comment(()) => panic!(),
        &Item::Import(ref n) => visitor.visit_name(n),
    }
}

fn walk_inductive<'v, V: Visitor<'v>>(visitor: &mut V, inductive: &'v Data) {
    visitor.visit_span(inductive.span);
    visitor.visit_name(&inductive.name);

    for &(ref n, ref t) in &inductive.parameters {
        visitor.visit_name(n);
        visitor.visit_term(t);
    }

    visitor.visit_term(&inductive.ty);

    for &(ref n, ref t) in &inductive.ctors {
        visitor.visit_name(n);
        visitor.visit_term(t);
    }
}

fn walk_def<'v, V: Visitor<'v>>(visitor: &mut V, def: &'v Function) {
    visitor.visit_span(def.span);
    visitor.visit_name(&def.name);

    for &(ref n, ref t) in &def.args {
        visitor.visit_name(n);
        visitor.visit_term(t);
    }

    visitor.visit_term(&def.ty);
    visitor.visit_term(&def.body);
}

fn walk_term<'v, V: Visitor<'v>>(visitor: &mut V, term: &'v Term) {
    use ast::Term::*;

    match term {
        &Literal { ref span, ref lit } =>
            panic!(),
        &Var { ref name } =>
            visitor.visit_name(name),
        &Match { ref span, ref scrutinee, ref cases } =>
            panic!(),
        &App { span, ref fun, ref arg } => {
            visitor.visit_span(span);
            visitor.visit_term(fun);
            visitor.visit_term(arg);
        }
        &Forall { span, ref name, ref ty, ref term } => {
            visitor.visit_span(span);
            visitor.visit_name(name);
            visitor.visit_term(ty);
            visitor.visit_term(term);
        }
        &Metavar { ref name } =>
            panic!(),
        &Lambda { span, ref args, ref ret_ty, ref body } => {
            visitor.visit_span(span);
            for &(ref n, ref t) in args {
                visitor.visit_name(n);
                visitor.visit_term(t);
            }
            visitor.visit_term(ret_ty);
            visitor.visit_term(body);
        }
        &Let { span, ref bindings, ref body } => {
            visitor.visit_span(span);
            panic!()
        }
        &Type => {}
    }
}

fn walk_name<'v, V: Visitor<'v>>(visitor: &mut V, name: &'v Name) {
    visitor.visit_span(name.span);
}
