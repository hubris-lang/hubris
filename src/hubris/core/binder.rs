use super::name::Name;
use super::term::Term;

extern crate pretty;
use self::pretty::*;

#[derive(Debug, Copy, Clone, PartialEq, Hash, Eq)]
pub enum BindingMode {
    Implicit,
    Explicit
}

#[derive(Debug, Clone, PartialEq, Hash, Eq)]
pub struct Binder {
    pub name: Name,
    pub ty: Box<Term>,
    pub mode: BindingMode,
}

impl Binder {
    pub fn explicit(name: Name, ty: Term) -> Binder {
        Binder {
            name: name,
            ty: Box::new(ty),
            mode: BindingMode::Explicit,
        }

    }

    pub fn implicit(name: Name, ty: Term) -> Binder {
        Binder {
            name: name,
            ty: Box::new(ty),
            mode: BindingMode::Implicit,
        }
    }


    pub fn with_mode(name: Name, ty: Term, mode: BindingMode) -> Binder {
        Binder {
            name: name,
            ty: Box::new(ty),
            mode: mode,
        }
    }

    pub fn abst(&self, index: usize, x: &Name) -> Binder {
        Binder {
            name: self.name.clone(),
            ty: Box::new(self.ty.abst(index, x)),
            mode: self.mode.clone(),
        }
    }

    pub fn replace(&self, index: usize, subst: &Term) -> Binder {
        Binder {
            name: self.name.clone(),
            ty: Box::new(self.ty.replace(index, subst)),
            mode: self.mode.clone(),
        }
    }

    pub fn is_implicit(&self) -> bool {
        match self.mode {
            BindingMode::Implicit => true,
            _ => false
        }
    }
}

impl Pretty for Binder {
    fn pretty(&self) -> Doc {
        if self.is_implicit() {
            braces(braces(self.name.pretty() + " : ".pretty() + self.ty.pretty()))
        } else {
            parens(self.name.pretty() + " : ".pretty() + self.ty.pretty())
        }
    }
}
