use std::fmt::{self, Debug, Display};
use std::path::{Path};
use super::core;
use pretty::*;

struct Module {
    //constructor: Vec<()>,
    definitions: Vec<Definition>,
}

struct Definition {
    name: core::Name,
    body: Term,
}

impl Pretty for Term {
    fn pretty(&self) -> Doc {
        let &Definition {
            ref name,
            ref body,
        } = self;

        "def ".pretty() + name.pretty() + " :=\n" + body.pretty()
    }
}

enum Term {
    Var(core::Name),
    Switch(Rc<Term>, ),
    Call(Rc<Term>, Vec<Term>),
    Lambda(Vec<core::Name>, Vec<Term>),
}

impl Pretty for Term {
    fn pretty(&self) -> Doc {
        panic!();
        // use self::Term::*;
        //
        // match self {
        //     &Var { ref name, .. } => name.pretty(),
        //     &App { ref fun, ref arg, .. } => {
        //         let pretty_fun = match &**fun {
        //             complex @ &Term::Lambda { .. } =>
        //                 parens(complex.pretty()),
        //             t => t.pretty()
        //         };
        //
        //         match &**arg {
        //             &Term::App { .. } => pretty_fun + " ".pretty() + parens(arg.pretty()),
        //             _ => pretty_fun + " ".pretty() + arg.pretty(),
        //         }
        //     }
        //     &Forall { ref binder, ref term, .. } => {
        //         if binder.name.is_placeholder() {
        //             let p = match &*binder.ty {
        //                 &Forall {..} => parens(binder.ty.pretty()) + " -> ".pretty(),
        //                 _ => binder.ty.pretty() + " -> ".pretty(),
        //             };
        //             p + term.pretty()
        //         } else {
        //             let mut cursor = &**term;
        //             let mut binders = Vec::new();
        //             binders.push(binder);
        //             while let &Term::Forall { ref binder, ref term, .. } = cursor {
        //                 // This is because we only want to pretty print the chunk of
        //                 // binders up to a placeholder name.
        //                 if binder.name.is_placeholder() { break; }
        //                 binders.push(binder);
        //                 cursor = term;
        //             }
        //             "forall ".pretty() + pretty_binders(binders.as_slice()) +
        //                 ", ".pretty() + cursor.pretty()
        //         }
        //     }
        //     &Lambda { ref binder, ref body, .. } => {
        //         // // This will be the term we will unroll binders from.
        //         // let mut term = &**body;
        //         //
        //         // // A list of coalesced binders
        //         // let mut cbinders = vec![];
        //         //
        //         // // Store the first binder's type
        //         // let mut binder_ty = &binder.ty;
        //         // let mut binders = vec![binder];
        //         // // If there is a sequence of binders then we want to coalesce
        //         // // them when printing like we can do in the syntax. The below
        //         // // loop will collect said binders.
        //         // while let &Term::Lambda { ref binder, ref body, .. } = term {
        //         //     if binder.ty == *binder_ty {
        //         //         binders.push(binder);
        //         //         term = &*body;
        //         //     } else {
        //         //         cbinders.push((binders, binder_ty));
        //         //         binder_ty = &binder.ty;
        //         //         binders = vec![];
        //         //     }
        //         // }
        //         //
        //         // // This code is ugly, lo
        //         // if cbinders.len() == 0 {
        //         //     cbinders.push((binders, binder_ty));
        //         // }
        //         //
        //         // // I think this code could probably be cleaner.
        //         // let mut coalesced_binder = "".pretty();
        //         // for (binders, ty) in cbinders {
        //         //     coalesced_binder = coalesced_binder + "(".pretty();
        //         //     for binder in binders {
        //         //         coalesced_binder = coalesced_binder + binder.name.pretty() + " ".pretty();
        //         //     }
        //         //     coalesced_binder = coalesced_binder + ": ".pretty() + ty.pretty();
        //         //     coalesced_binder = coalesced_binder + ") ".pretty();
        //         // }
        //         // Now we pretty print the function with the collesced binders.
        //         let mut cursor = &**body;
        //         let mut binders = Vec::new();
        //         binders.push(binder);
        //         while let &Term::Lambda { ref binder, ref body, .. } = cursor {
        //             // This is because we only want to pretty print the chunk of
        //             // binders up to a placeholder name.
        //             if binder.name.is_placeholder() { break; }
        //             binders.push(binder);
        //             cursor = body;
        //         }
        //
        //         "fun ".pretty() + pretty_binders(binders.as_slice()) + " => ".pretty() + cursor.pretty()
        //     }
        //     &Type => Doc::text("Type"),
        // }
    }
}

impl Display for Term {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        format(self, formatter)
    }
}


fn lower_module(module: core::Module) -> Module {
    Module {
        definitions:
            module.defs
                  .into_iter()
                  .filter_map(|i| match i {
                      core::Item::Fn(d) => Some(lower_def(d)),
                      _ => None,
                  })
                  .collect()
    }
}

fn lower_def(def: core::Def) -> Definition {
    panic!("{}", def);
}


impl Backend for Rust {
    fn create_executable<P: AsRef<Path> + Debug>(module: core::Module, output: Option<P>) {
        let m = lower_module(module);
        for def in m.defs {
            println!("{}", def.pretty())
        }
    }
}
