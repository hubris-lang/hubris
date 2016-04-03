use std::fmt::{self, Debug, Formatter, Display};
use std::fs::File;
use std::path::{Path};
use std::rc::Rc;
use std::io::Write;
use super::core;
use super::typeck::TyCtxt;
use pretty::*;

/// A trait that describes the interface to a particular compiler backend.
pub trait Backend {
    fn create_executable<P: AsRef<Path> + Debug>(main: core::Definition, ty_cx: TyCtxt, output: Option<P>);
}

pub struct Rust;

impl Backend for Rust {
    fn create_executable<P: AsRef<Path> + Debug>(main: core::Definition, ty_cx: TyCtxt, output: Option<P>) {
        let mut erasure_cx = ErasureCx::new(&ty_cx);
        let mut output_file = File::create(output.unwrap()).unwrap();

        // First we declare the runtime as an external crate, and bring all
        // of its types and functions into scope.
        output_file.write(&"extern crate rt;\n\nuse rt::*;\n\n".as_bytes()[..]);

        let mut definitions = vec![];

        // We then loop through the types creating definitions for the types and constructors.
        //
        // After we fully implement type erasure we should be able to remove the need to ever
        // have the types exists as runtime values.
        //
        // Currently we just generate panics for their bodies since evaluating code like
        // this should be a bug.
        for (name, data) in &ty_cx.types {
            println!("data: {}", name);
            definitions.push(Definition {
                name: name.clone(),
                body: Term::Panic("a".to_string())
            });
        }

        for (n, axiom) in &ty_cx.axioms {
            println!("axiom: {}", n);
        }

        for (n, def) in &ty_cx.definitions {
            definitions.push(erasure_cx.lower_def(def.clone()));
        }

        // We have now produced a set of definitions that we then convert to
        // Rust code and write to the output file.
        for def in definitions {
            println!("-----------(lowered)-----------------");
            println!("{}", def);
            println!("-----------(rust)-----------------");
            let rust_code = def_to_rust(&def);
            let mut v = Vec::new();
            Doc::render(&rust_code, 80, &mut v).unwrap();
            output_file.write(&v[..]);
            output_file.write(&"\n".as_bytes[..]);
            println!("{}", String::from_utf8(v).unwrap());
        }
    }
}

fn name_to_rust(name: &core::Name) -> Doc {
    match name {
        &core::Name::Qual { ref components, .. } => {
            let pieces: Vec<_> =
                components.iter()
                          .map(|c| c.pretty())
                          .collect();
            seperate(&pieces[..], &"_".pretty())
        }
        &core::Name::DeBruijn { ref repr, .. } => repr.pretty(),
        &core::Name::Local { ref repr, .. } => repr.pretty(),
        &core::Name::Meta { .. } => panic!(),
    }
}

fn def_to_rust(def: &Definition) -> Doc {
    let (args, body) = match &def.body {
        &Term::Lambda(ref ns, ref body) => {
            let args : Vec<_> =
                ns.iter()
                  .map(|n| name_to_rust(n) + ": Obj".pretty())
                  .collect();

            (args, &**body)
        }
        t => (vec![], t)
    };

    "fn ".pretty() +
    name_to_rust(&def.name) +
    parens(seperate(&args[..], &",".pretty())) + " -> Obj {\n".pretty() +
        term_to_rust(body) + "\n".pretty() +
    "}\n".pretty()
}

fn block(value: Doc) -> Doc {
    "{".pretty() + Doc::newline() +
        value.nest(4) +
    "}".pretty()
}

fn to_object(value: Doc) -> Doc {
    "Obj::from".pretty() + parens(value)
}

fn term_to_rust(term: &Term) -> Doc {
    match term {
        &Term::Call(ref f, ref args) => {
            let args : Vec<_> = args.iter().map(|x| term_to_rust(x)).collect();
            term_to_rust(&**f) + parens(seperate(&args[..], &",".pretty()))
        }
        &Term::Var(ref name) => name_to_rust(name),
        &Term::Lambda(ref ns, ref body) => {
            let args : Vec<_> =
                ns.iter()
                  .map(|n| name_to_rust(n) + ": Obj".pretty())
                  .collect();
            to_object("|".pretty() + seperate(&args[..], &",".pretty()) + "|".pretty() +
                block(term_to_rust(body)))
        }
        &Term::Panic(ref msg) => {
            "panic!".pretty() + parens("\"".pretty() + msg.pretty() + "\"".pretty())
        }
        t => panic!("{:?}", t),
    }
}

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

#[derive(Debug, Clone)]
enum Term {
    Local(core::Name, usize),
    Var(core::Name),
    // Free(core::)
    Switch(Rc<Term>),
    Call(Rc<Term>, Vec<Term>),
    Lambda(Vec<core::Name>, Box<Term>),
    Panic(String),
}

impl Pretty for Term {
    fn pretty(&self) -> Doc {
        use self::Term::*;

        match self {
            &Local(_, i) => panic!(),
            &Var(ref name) => name.pretty(),
            &Switch(ref scrut) => panic!(),
            &Call(ref f, ref args) => {
                let pargs =
                    args.iter()
                        .map(|x| x.pretty())
                        .collect::<Vec<_>>();

                f.pretty() + parens(seperate(&pargs[..], &",".pretty()))
            }
            &Lambda(_, ref body) => body.pretty(),
            &Panic(_) => "panic".pretty(),
        }
    }
}

impl Display for Term {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        format(self, formatter)
    }
}

/// This context is used to do type erasure, and lowering of `core::Term` to an
/// untyped lambda calculus.
struct ErasureCx<'tcx> {
    ty_cx: &'tcx TyCtxt
}

impl<'tcx> ErasureCx<'tcx> {
    pub fn new(ty_cx: &'tcx TyCtxt) -> ErasureCx<'tcx> {
        ErasureCx {
            ty_cx: ty_cx
        }
    }

//     fn lower_module(module: core::Module) -> Module {
//     Module {
//         definitions:
//             module.defs
//                   .into_iter()
//                   .filter_map(|i| match i {
//                       core::Item::Fn(d) => Some(lower_def(d)),
//                       _ => None,
//                   })
//                   .collect()
//     }
// }

    fn lower_def(&mut self, def: core::Definition) -> Definition {
        let core::Definition {
            name,
            args,
            ty,
            body,
            reduction,
        } = def;

        println!("name: {}", name);
        println!("ty: {}", ty);
        println!("body: {}", body);

        let def = Definition {
            name: name,
            body: self.lower_term(body),
        };

        println!("def: {}", def);

        def
    }

    fn lower_term(&mut self, term: core::Term) -> Term {
        match term {
            lam @ core::Term::Lambda { .. } => {
                let mut final_body = lam;
                let mut names = vec![];
                while let core::Term::Lambda { binder, body, .. } = final_body {
                    println!("binder: {} {}",
                    binder.name, binder.ty);
                    names.push(binder.name.clone());
                    final_body = *body;
                }
                Term::Lambda(names, Box::new(self.lower_term(final_body)))
            }
            app @ core::Term::App { .. } => {
                let (head, args) = app.uncurry();
                println!("head: {}", head);
                let lhead = self.lower_term(head);
                for arg in &args {
                    println!("args: {}", arg);
                }
            Term::Call(Rc::new(lhead),
                       args.into_iter()
                           .map(|arg| self.lower_term(arg))
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
}
