use super::super::ast::*;

pub trait Visitor<'v> : Sized {
    fn visit_module(&mut self, module: &'v Module) {
        walk_module(self, module)
    }

    fn visit_item(&mut self, item: &'v Item) {
        walk_item(self, item)
    }

    fn visit_data(&mut self, inductive: &'v Inductive) {
        walk_inductive(self, inductive)
    }

    fn visit_extern(&mut self, ext: &'v Extern) {
        walk_extern(self, ext)
    }

    fn visit_def(&mut self, def: &'v Def) {
        walk_def(self, def)
    }

    fn visit_axiom(&mut self, a: &'v Axiom) {
        walk_axiom(self, a)
    }

    fn visit_term(&mut self, term: &'v Term) {
        walk_term(self, term)
    }

    fn visit_binder(&mut self, binder: &'v Binder) {
        walk_binder(self, binder)
    }

    fn visit_span(&mut self, _span: &'v Span) {}

    fn visit_case(&mut self, case: &'v Case) {
        walk_case(self, case)
    }

    fn visit_pattern(&mut self, pattern: &'v Pattern) {
        walk_pattern(self, pattern)
    }

    fn visit_literal(&mut self, _lit: &'v Literal) {
    }

    fn visit_name(&mut self, name: &'v Name) {
        walk_name(self, name)
    }
}

pub fn walk_module<'v, V: Visitor<'v>>(visitor: &mut V, module: &'v Module) {
    visitor.visit_span(&module.span);
    visitor.visit_name(&module.name);

    for item in &module.items {
        visitor.visit_item(item);
    }
}

pub fn walk_item<'v, V: Visitor<'v>>(visitor: &mut V, item: &'v Item) {
    match item {
        &Item::Inductive(ref d) => visitor.visit_data(d),
        &Item::Def(ref def) => visitor.visit_def(def),
        &Item::Axiom(ref a) => visitor.visit_axiom(a),
        &Item::Extern(ref ext) => visitor.visit_extern(ext),
        &Item::Comment(ref _s) => panic!(),
        &Item::Import(ref n) => visitor.visit_name(n),
    }
}

pub fn walk_inductive<'v, V: Visitor<'v>>(visitor: &mut V, inductive: &'v Inductive) {
    visitor.visit_span(&inductive.span);
    visitor.visit_name(&inductive.name);

    for binder in &inductive.parameters {
        visitor.visit_binder(binder);
    }

    visitor.visit_term(&inductive.ty);

    for &(ref n, ref t) in &inductive.ctors {
        visitor.visit_name(n);
        visitor.visit_term(t);
    }
}

pub fn walk_def<'v, V: Visitor<'v>>(visitor: &mut V, def: &'v Def) {
    visitor.visit_span(&def.span);
    visitor.visit_name(&def.name);

    for binder in &def.args {
        visitor.visit_binder(binder);
    }

    visitor.visit_term(&def.ty);
    visitor.visit_term(&def.body);
}

pub fn walk_axiom<'v, V: Visitor<'v>>(visitor: &mut V, a: &'v Axiom) {
    visitor.visit_span(&a.span);
    visitor.visit_name(&a.name);
    visitor.visit_term(&a.ty);
}

pub fn walk_extern<'v, V: Visitor<'v>>(visitor: &mut V, ext: &'v Extern) {
    visitor.visit_span(&ext.span);
    visitor.visit_name(&ext.name);
    visitor.visit_term(&ext.term);
}

pub fn walk_term<'v, V: Visitor<'v>>(visitor: &mut V, term: &'v Term) {
    use ast::Term::*;

    match term {
        &Literal { ref span, ref lit } => {
            visitor.visit_span(span);
            visitor.visit_literal(lit);
        }
        &Var { ref name } =>
            visitor.visit_name(name),
        &Match { ref span, ref scrutinee, ref cases } => {
            visitor.visit_span(span);
            visitor.visit_term(scrutinee);
            for case in cases {
                visitor.visit_case(case);
            }
        },
        &App { ref span, ref fun, ref arg } => {
            visitor.visit_span(span);
            visitor.visit_term(fun);
            visitor.visit_term(arg);
        }
        &Forall { ref span, ref binders, ref term } => {
            visitor.visit_span(span);
            for binder in binders {
                visitor.visit_binder(binder);
            }

            visitor.visit_term(term);
        }
        &Lambda { ref span, ref args, ref ret_ty, ref body } => {
            visitor.visit_span(&span);
            for binder in args {
                visitor.visit_binder(binder);
            }

            match **ret_ty {
                None => {}
                Some(ref rt) =>
                    visitor.visit_term(rt),
            }

            visitor.visit_term(body);
        }
        &Let { ref span, ref bindings, ref body } => {
            visitor.visit_span(span);
            panic!()
        }
        &Type => {}
    }
}

pub fn walk_case<'v, V: Visitor<'v>>(visitor: &mut V, case: &'v Case) {
    let &Case {
        ref span,
        ref pattern,
        ref rhs,
    } = case;

    visitor.visit_span(span);
    visitor.visit_pattern(pattern);
    visitor.visit_term(rhs);
}

pub fn walk_pattern<'v, V: Visitor<'v>>(visitor: &mut V, pattern: &'v Pattern) {
    use ast::Pattern::*;

    match pattern {
        &Name(ref name) => visitor.visit_name(name),
        &Constructor(ref name, ref pats) => {
            visitor.visit_name(name);
            for pat in pats {
                visitor.visit_pattern(pat);
            }
        }
        &Placeholder => {}
    }
}


pub fn walk_name<'v, V: Visitor<'v>>(visitor: &mut V, name: &'v Name) {
    visitor.visit_span(&name.span);
}

pub fn walk_binder<'v, V: Visitor<'v>>(visitor: &mut V, binder: &'v Binder) {
    visitor.visit_span(&binder.span);
    for name in &binder.names {
        visitor.visit_name(name);
    }
    binder.ty.as_ref().map(|ty| visitor.visit_term(ty));
}
