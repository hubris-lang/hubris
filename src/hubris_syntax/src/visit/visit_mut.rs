use super::super::ast::*;

pub trait VisitorMut<'v> : Sized {
    fn visit_mut_module(&mut self, module: &'v mut Module) {
        walk_mut_module(self, module)
    }

    fn visit_mut_item(&mut self, item: &'v mut Item) {
        walk_mut_item(self, item)
    }

    fn visit_mut_data(&mut self, inductive: &'v mut Inductive) {
        walk_mut_inductive(self, inductive)
    }

    fn visit_mut_extern(&mut self, ext: &'v mut Extern) {
        walk_mut_extern(self, ext)
    }

    fn visit_mut_def(&mut self, def: &'v mut Def) {
        walk_mut_def(self, def)
    }

    fn visi_mut_axiom(&mut self, a: &'v mut Axiom) {
        walk_mut_axiom(self, a)
    }

    fn visit_mut_term(&mut self, term: &'v mut Term) {
        walk_mut_term(self, term)
    }

    fn visit_mut_binder(&mut self, binder: &'v mut Binder) {
        walk_mut_binder(self, binder)
    }

    fn visit_mut_span(&mut self, _span: &'v mut Span) {}

    fn visit_mut_case(&mut self, case: &'v mut Case) {
        walk_mut_case(self, case);
    }

    fn visit_mut_pattern(&mut self, pattern: &'v mut Pattern) {
        walk_mut_pattern(self, pattern)
    }

    fn visit_mut_literal(&mut self, _lit: &'v mut Literal) {}

    fn visit_mut_name(&mut self, name: &'v mut Name) {
        walk_mut_name(self, name)
    }
}

pub fn walk_mut_module<'v, V: VisitorMut<'v>>(visitor: &mut V, module: &'v mut Module) {
    visitor.visit_mut_span(&mut module.span);
    visitor.visit_mut_name(&mut module.name);

    for item in &mut module.items {
        visitor.visit_mut_item(item);
    }
}

pub fn walk_mut_item<'v, V: VisitorMut<'v>>(visitor: &mut V, item: &'v mut Item) {
    match item {
        &mut Item::Inductive(ref mut d) => visitor.visit_mut_data(d),
        &mut Item::Def(ref mut def) => visitor.visit_mut_def(def),
        &mut Item::Axiom(ref mut a) => visitor.visi_mut_axiom(a),
        &mut Item::Extern(ref mut ext) => panic!(),
        &mut Item::Comment(ref mut s) => panic!(),
        &mut Item::Import(ref mut n) => visitor.visit_mut_name(n),
    }
}

pub fn walk_mut_inductive<'v, V: VisitorMut<'v>>(visitor: &mut V, inductive: &'v mut Inductive) {
    visitor.visit_mut_span(&mut inductive.span);
    visitor.visit_mut_name(&mut inductive.name);

    for binder in &mut inductive.parameters {
        visitor.visit_mut_binder(binder);
    }

    visitor.visit_mut_term(&mut inductive.ty);

    for &mut (ref mut n, ref mut t) in &mut inductive.ctors {
        visitor.visit_mut_name(n);
        visitor.visit_mut_term(t);
    }
}

pub fn walk_mut_def<'v, V: VisitorMut<'v>>(visitor: &mut V, def: &'v mut Def) {
    visitor.visit_mut_span(&mut def.span);
    visitor.visit_mut_name(&mut def.name);

    for binder in &mut def.args {
        visitor.visit_mut_binder(binder);
    }

    visitor.visit_mut_term(&mut def.ty);
    visitor.visit_mut_term(&mut def.body);
}

pub fn walk_mut_axiom<'v, V: VisitorMut<'v>>(visitor: &mut V, a: &'v mut Axiom) {
    visitor.visit_mut_span(&mut a.span);
    visitor.visit_mut_name(&mut a.name);
    visitor.visit_mut_term(&mut a.ty);
}

pub fn walk_mut_extern<'v, V: VisitorMut<'v>>(visitor: &mut V, ext: &'v mut Extern) {
    visitor.visit_mut_span(&mut ext.span);
    visitor.visit_mut_name(&mut ext.name);
    visitor.visit_mut_term(&mut ext.term);
}

pub fn walk_mut_term<'v, V: VisitorMut<'v>>(visitor: &mut V, term: &'v mut Term) {
    use ast::Term::*;

    match term {
        &mut Literal { ref mut span, ref mut lit } => {
            visitor.visit_mut_span(span);
            visitor.visit_mut_literal(lit);
        }
        &mut Var { ref mut name } =>
            visitor.visit_mut_name(name),
        &mut Match { ref mut span, ref mut scrutinee, ref mut cases } => {
            visitor.visit_mut_span(span);
            visitor.visit_mut_term(scrutinee);
            for case in cases {
                visitor.visit_mut_case(case);
            }
        }

        &mut App { ref mut span, ref mut fun, ref mut arg } => {
            visitor.visit_mut_span(span);
            visitor.visit_mut_term(fun);
            visitor.visit_mut_term(arg);
        }
        &mut Forall { ref mut span, ref mut binders, ref mut term } => {
            visitor.visit_mut_span(span);
            for binder in binders {
                visitor.visit_mut_binder(binder);
            }

            visitor.visit_mut_term(term);
        }
        &mut Lambda { ref mut span, ref mut args, ref mut ret_ty, ref mut body } => {
            visitor.visit_mut_span(span);
            for binder in args {
                visitor.visit_mut_binder(binder);
            }

            match **ret_ty {
                None => {}
                Some(ref mut rt) =>
                    visitor.visit_mut_term(rt),
            }

            visitor.visit_mut_term(body);
        }
        &mut Let { ref mut span, ref mut bindings, ref mut body } => {
            visitor.visit_mut_span(span);
            panic!()
        }
        &mut Type => {}
    }
}

pub fn walk_mut_case<'v, V: VisitorMut<'v>>(visitor: &mut V, case: &'v mut Case) {
    let &mut Case {
        ref mut span,
        ref mut pattern,
        ref mut rhs,
    } = case;

    visitor.visit_mut_span(span);
    visitor.visit_mut_pattern(pattern);
    visitor.visit_mut_term(rhs);
}

pub fn walk_mut_pattern<'v, V: VisitorMut<'v>>(visitor: &mut V, pattern: &'v mut Pattern) {
    use ast::Pattern::*;

    match pattern {
        &mut Name(ref mut name) => visitor.visit_mut_name(name),
        &mut Constructor(ref mut name, ref mut pats) => {
            visitor.visit_mut_name(name);
            for pat in pats {
                visitor.visit_mut_pattern(pat);
            }
        }
        &mut Placeholder => {}
    }
}

pub fn walk_mut_name<'v, V: VisitorMut<'v>>(visitor: &mut V, name: &'v mut Name) {
    visitor.visit_mut_span(&mut name.span);
}

pub fn walk_mut_binder<'v, V: VisitorMut<'v>>(visitor: &mut V, binder: &'v mut Binder) {
    visitor.visit_mut_span(&mut binder.span);
    for name in &mut binder.names {
        visitor.visit_mut_name(name);
    }
    binder.ty.as_mut().map(|ty| visitor.visit_mut_term(ty));
}
