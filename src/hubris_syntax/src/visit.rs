use super::ast::*;

pub enum Mut {}
pub enum Imm {}

trait MutType<'v, T> {
    type Output;
}

impl<'v, T: 'v> MutType<'v, T> for Mut {
    type Output = &'v mut T;
}

impl<'v, T: 'v> MutType<'v, T> for Imm {
    type Output = &'v T;
}

pub trait Ref<'v, M: MutType<'v, Self>> : Sized {
    type T;
}

impl<'v, T: 'v, M: MutType<'v, T>> Ref<'v, M> for T {
    type T = M::Output;
}

// fn as_ref<'v, M, U: Ref<'v, M>>(x: U) -> U::T {
//     panic!()
// }

pub trait Visitor<'v, M> : Sized {
    fn visit_module(&mut self, module: <Module as Ref<'v, M>>::T)
    where M : MutType<'v, Module> {
        walk_module(self, module)
    }

    fn visit_item(&mut self, item: <Item as Ref<'v, M>>::T)
    where M : MutType<'v, Item> {
        walk_item(self, item)
    }

    fn visit_data(&mut self, inductive: <Inductive as Ref<'v, M>>::T)
    where M : MutType<'v, Inductive> {
        walk_inductive(self, inductive)
    }

    fn visit_extern(&mut self, ext: <Extern as Ref<'v, M>>::T)
    where M : MutType<'v, Extern> {
        walk_extern(self, ext)
    }

    fn visit_def(&mut self, def: <Def as Ref<'v, M>>::T)
    where M : MutType<'v, Def> {
        walk_def(self, def)
    }

    fn visit_term(&mut self, term: <Term as Ref<'v, M>>::T)
    where M : MutType<'v, Term> {
        walk_term(self, term)
    }

    fn visit_binder(&mut self, binder: <Binder as Ref<'v, M>>::T)
    where M : MutType<'v, Binder> {
        walk_binder(self, binder)
    }

    fn visit_span(&mut self, span: <Span as Ref<'v, M>>::T)
    where M : MutType<'v, Span> {}

    fn visit_case(&mut self, _case: <Case as Ref<'v, M>>::T)
    where M : MutType<'v, Case> {
        panic!();
    }

    fn visit_pattern(&mut self, _pattern: <Pattern as Ref<'v, M>>::T)
    where M : MutType<'v, Pattern> {
        panic!();
    }

    fn visit_literal(&mut self, _lit: <Literal as Ref<'v, M>>::T)
    where M : MutType<'v, Literal> {
        panic!();
    }

    fn visit_name(&mut self, name: <Name as Ref<'v, M>>::T)
    where M : MutType<'v, Name> {
        walk_name(self, name)
    }
}

pub fn walk_module<'v, M: MutType<'v, Module>, V: Visitor<'v, M>>(
    visitor: &mut V,
    module: <Module as Ref<'v, M>>::T) {

    visitor.visit_span(module.span);
    visitor.visit_name(&module.name);

    for item in &module.items {
        visitor.visit_item(item);
    }
}

pub fn walk_item<'v, M: MutType<'v, Item>, V: Visitor<'v, M>>(
    visitor: &mut V,
    item: <Item as Ref<'v, M>>::T) {
    use ast::Item::*;

    match item {
        &Item::Inductive(ref d) => visitor.visit_data(d),
        &Item::Def(ref def) => visitor.visit_def(def),
        &Item::Extern(ref ext) => visitor.visit_extern(ext),
        &Item::Comment(ref s) => panic!(),
        &Item::Import(ref n) => visitor.visit_name(n),
    }
}

pub fn walk_inductive<'v, M: MutType<'v, Inductive>, V: Visitor<'v, M>>(
    visitor: &mut V,
    inductive: <Inductive as Ref<'v, M>>::T) {
    visitor.visit_span(inductive.span);
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

pub fn walk_def<'v, M: MutType<'v, Def>, V: Visitor<'v, M>>(visitor: &mut V, def: <Def as Ref<'v, M>>::T) {
    visitor.visit_span(def.span);
    visitor.visit_name(&def.name);

    for binder in &def.args {
        visitor.visit_binder(binder);
    }

    visitor.visit_term(&def.ty);
    visitor.visit_term(&def.body);
}

pub fn walk_extern<'v, M: MutType<'v, Extern>, V: Visitor<'v, M>>(
        visitor: &mut V,
        ext: <Extern as Ref<'v, M>>::T) {
    visitor.visit_span(ext.span);
    visitor.visit_name(&ext.name);
    visitor.visit_term(&ext.term);
}

pub fn walk_term<'v, M: MutType<'v, Term>, V: Visitor<'v, M>>(
        visitor: &mut V,
        term: <Term as Ref<'v, M>>::T) {
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
        &Forall { span, ref binders, ref term } => {
            visitor.visit_span(span);
            for binder in binders {
                visitor.visit_binder(binder);
            }

            visitor.visit_term(term);
        }
        &Lambda { span, ref args, ref ret_ty, ref body } => {
            visitor.visit_span(span);
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
        &Let { span, ref bindings, ref body } => {
            visitor.visit_span(span);
            for &(ref binding, ref body) in bindings {
                visitor.visit_binder(binding);
                visitor.visit_term(body);
            }
            visitor.visit_term(body);
        }
        &Type => {}
    }
}

pub fn walk_name<'v, M: MutType<'v, Name>, V: Visitor<'v, M>>(visitor: &mut V, name: <Name as Ref<'v, M>>::T) {
    visitor.visit_span(name.span);
}

pub fn walk_binder<'v, M: MutType<'v, Binder>, V: Visitor<'v, M>>(
    visitor: &mut V,
    binder: <Binder as Ref<'v, M>>::T) {
    visitor.visit_span(binder.span);
    visitor.visit_name(&binder.name);
    visitor.visit_term(&binder.ty);
}
