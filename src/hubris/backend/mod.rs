use std::fmt::{self, Debug, Formatter, Display};
use std::path::{Path};
use std::rc::Rc;
use super::core;
use pretty::*;

/// A trait that describes the interface to a particular compiler backend.
pub trait Backend {
    fn create_executable<P: AsRef<Path> + Debug>(module: core::Module, output: Option<P>);
}

pub struct Rust;

struct Module {
    //constructor: Vec<()>,
    definitions: Vec<Definition>,
}

struct Definition {
    name: core::Name,
    body: Term,
}

impl Pretty for Definition {
    fn pretty(&self) -> Doc {
        let &Definition {
            ref name,
            ref body,
        } = self;

        "def ".pretty() + name.pretty() + " :=\n".pretty() + body.pretty()
    }
}

impl Display for Definition {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        format(self, formatter)
    }
}

enum Term {
    Local(usize),
    Var(core::Name),
    // Free(core::)
    Switch(Rc<Term>),
    Call(Rc<Term>, Vec<Term>),
    Lambda(Vec<core::Name>, Box<Term>),
}

impl Pretty for Term {
    fn pretty(&self) -> Doc {
        use self::Term::*;

        match self {
            &Local(i) => panic!(),
            &Var(ref name) => name.pretty(),
            &Switch(ref scrut) => panic!(),
            &Call(ref f, ref args) => {
                let pargs =
                    args.iter()
                        .map(|x| x.pretty())
                        .fold("".pretty(),|a,i| a + i);

                f.pretty() + parens(pargs)
            }
            &Lambda(_, ref body) => body.pretty(),
        }
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
    let core::Def {
        name,
        args,
        ret_ty,
        body,
    } = def;

    println!("name: {}", name);
    println!("ty: {}", ret_ty);
    println!("body: {}", body);

    let def = Definition {
        name: name,
        body: lower_term(body),
    };

    println!("def: {}", def);

    def
}

fn lower_term(term: core::Term) -> Term {
    match term {
        core::Term::Lambda { binder, body, .. } => {
            println!("binder: {} {}",
            binder.name, binder.ty);
            lower_term(*body)
        }
        app @ core::Term::App { .. } => {
            let (head, args) = app.uncurry();
            println!("head: {}", head);
            let lhead = lower_term(head);
            for arg in &args {
                println!("args: {}", arg);
            }
            Term::Call(Rc::new(lhead),
                       args.into_iter()
                           .map(|arg| lower_term(arg))
                           .collect())
        }
        core::Term::Var { name } => {
            println!("name: {}", name);
            match name {
                core::Name::Qual { .. } => {
                    Term::Var(name)
                },
                n => Term::Var(n),
                //l => panic!("{}", l)
            }
        }
        _ => panic!()
    }
}


impl Backend for Rust {
    fn create_executable<P: AsRef<Path> + Debug>(module: core::Module, output: Option<P>) {
        let m = lower_module(module);
        for def in m.definitions {
            println!("{}", def)
        }
    }
}
