use ast::{self, SourceMap};
use core;
use typeck::TyCtxt;

use std::collections::{HashMap, HashSet};
use std::path::Path;

#[derive(Clone, Debug)]
pub enum Error {
    UnexpectedQualifiedName,
    UnknownVariable(ast::Name),
    InvalidImport,
    Many(Vec<Error>),
}

pub struct ElabCx {
    module: ast::Module,
    /// The set of declared constructor names in scope
    /// we need this to differentiate between a name
    /// binding or null-ary constructor in pattern
    /// matching.
    constructors: HashSet<ast::Name>,
    globals: HashMap<ast::Name, core::Name>,
    pub ty_cx: TyCtxt,
}

impl ElabCx {
    pub fn from_module(module: ast::Module, source_map: SourceMap) -> ElabCx {
        // Elaboration relies on type checking, so first we setup a typing context to use
        // while elaborating the program. We will run the type checker in inference mode
        // to setup constraints, and then we will solve the constraints, and check the
        // term again.
        let mut ty_cx = TyCtxt::empty();
        ty_cx.source_map = source_map;

        ElabCx {
            module: module,
            constructors: HashSet::new(),
            globals: HashMap::new(),
            ty_cx: ty_cx,
        }
    }

    pub fn elaborate_module<P: AsRef<Path>>(&mut self, path: P) -> Result<core::Module, Error> {
        let ecx = self;
        let module_name = ecx.module.name.clone();

        let name = try!(ecx.elaborate_global_name(module_name));

        let mut errors = vec![];
        let mut defs = vec![];
        let mut imports = vec![];

        for def in ecx.module.items.clone() {
            match &def {
                &ast::Item::Data(ref d) => {
                    for ctor in &d.ctors {
                        ecx.constructors.insert(ctor.0.clone());
                    }
                },
                &ast::Item::Import(ref n) => {
                    imports.push(try!(ecx.elaborate_import(n.clone())))
                }
                _ => {}
            }


            match ecx.elaborate_def(def) {
                Err(e) => errors.push(e),
                Ok(edef) => {
                    let edef = edef.map(|edef| {
                        match &edef {
                            &core::Item::Data(ref d) =>
                                ecx.ty_cx.declare_datatype(d),
                            &core::Item::Fn(ref f) =>
                                ecx.ty_cx.declare_def(f),
                            &core::Item::Extern(ref e) =>
                                ecx.ty_cx.declare_extern(e),
                        }
                        edef
                    });

                    match edef {
                        None => {}
                        Some(edef) => defs.push(edef),
                    }
                }
            }
        }

        if errors.len() != 0 {
            Err(Error::Many(errors))
        } else {
            Ok(core::Module {
                file_name: path.as_ref().to_owned(),
                name: name,
                defs: defs,
                imports: imports,
            })
        }
    }

    pub fn elaborate_import(&mut self, name: ast::Name) -> Result<core::Name, Error> {
        match name.repr {
            ast::NameKind::Qualified(components) => Ok(core::Name::Qual {
                components: components,
                span: name.span,
            }),
            ast::NameKind::Unqualified(..) => Err(Error::InvalidImport),
        }
    }

    pub fn elaborate_def(&mut self, def: ast::Item) -> Result<Option<core::Item>, Error> {
        debug!("elaborate_def: def={:?}", def);

        match def {
            ast::Item::Data(d) => {
                let edata = core::Item::Data(try!(self.elaborate_data(d)));
                Ok(Some(edata))
            }
            ast::Item::Fn(f) => {
                let efn = core::Item::Fn(try!(self.elaborate_fn(f)));
                debug!("elaborate_def: fn={}", efn);
                Ok(Some(efn))
            }
            ast::Item::Extern(e) => {
                let ext = core::Item::Extern(try!(self.elaborate_extern(e)));
                Ok(Some(ext))
            }
            ast::Item::Comment(_) |
            ast::Item::Import(_) => Ok(None)
        }
    }

    fn elaborate_data(&mut self, data: ast::Data) -> Result<core::Data, Error> {
        let rec_name = data.name.in_scope("rec".to_string()).unwrap();
        let ty_name = try!(self.elaborate_global_name(data.name));

        // Pre-declare the recursor for the time being.
        self.globals.insert(
            rec_name,
            ty_name.in_scope("rec".to_string()).unwrap());

        let mut lcx = LocalElabCx::from_elab_cx(self);
        let mut ctors = Vec::new();

        for ctor in data.ctors.into_iter() {
            let ector = try!(lcx.elaborate_ctor(ctor));
            ctors.push(ector);
        }

        Ok(core::Data {
            span: data.span,
            name: ty_name,
            ty: try!(lcx.elaborate_term(data.ty)),
            ctors: ctors,
        })
    }

    fn elaborate_fn(&mut self, fun: ast::Function) -> Result<core::Function, Error> {
        let mut lcx = LocalElabCx::from_elab_cx(self);

        lcx.enter_scope(fun.args.clone(), move |lcx, args| {
            let name = try!(lcx.cx.elaborate_global_name(fun.name));
            let ty = try!(lcx.elaborate_term(fun.ty.clone()));
            let ebody = try!(lcx.elaborate_term(fun.body));

            debug!("elaborate_fn: ty={} body={}", ty, ebody);

            Ok(core::Function {
                name: name,
                args: args.clone(),
                // We compute the full type of the function here
                // by taking the return type and abstracting
                // over the arguments.
                ret_ty: core::Term::abstract_pi(args.clone(), ty),
                // We construct a lambda representing the body
                // with all of the function's parameters abstracted.
                body: core::Term::abstract_lambda(args, ebody),
            })
        })
    }

    fn elaborate_extern(&mut self, ext: ast::Extern) -> Result<core::Extern, Error> {
        let ast::Extern { span, name, term } = ext;
        Ok(core::Extern {
            span: span,
            name: try!(self.elaborate_global_name(name)),
            term: try!(LocalElabCx::from_elab_cx(self)
                                  .elaborate_term(term)),
        })
    }

    fn elaborate_global_name(&mut self, n: ast::Name) -> Result<core::Name, Error> {
        match n.repr.clone() {
            ast::NameKind::Qualified(_) =>
                Err(Error::UnexpectedQualifiedName),
            ast::NameKind::Unqualified(name) => {
                let qn = core::Name::Qual {
                    span: n.span,
                    components: vec![name]
                };

                self.globals.insert(n.clone(), qn.clone());

                Ok(qn)
            }
        }
    }
}

pub struct LocalElabCx<'ecx> {
    cx: &'ecx mut ElabCx,
    locals: HashMap<ast::Name, core::Name>,
    metavar_counter: usize,
}

impl<'ecx> LocalElabCx<'ecx>  {
    pub fn from_elab_cx(ecx: &'ecx mut ElabCx) -> LocalElabCx<'ecx> {
        LocalElabCx {
            cx: ecx,
            locals: HashMap::new(),
            metavar_counter: 0
        }
    }

    fn enter_scope<F, R>(&mut self, binders: Vec<(ast::Name, ast::Term)>, body: F) -> Result<R, Error>
    where F : FnOnce(&mut LocalElabCx, Vec<core::Name>) -> Result<R, Error> {
        let mut locals = vec![];

        let old_context = self.locals.clone();

        for (name, t) in binders {
            let repr = match name.clone().repr {
                ast::NameKind::Qualified(..) => panic!(),
                ast::NameKind::Unqualified(s) => s,
            };

            let eterm = try!(self.elaborate_term(t));
            let local = self.cx.ty_cx.local_with_repr(repr, eterm.clone());

            self.locals.insert(name, local.clone());
            locals.push(local);
        }

        let result = try!(body(self, locals));

        self.locals = old_context;

        Ok(result)
    }

    fn elaborate_ctor(&mut self, ctor: ast::Constructor) -> Result<(core::Name, core::Term), Error> {
        let ename = try!(self.cx.elaborate_global_name(ctor.0));
        let ety = try!(self.elaborate_term(ctor.1));

        Ok((ename, ety))
    }

    pub fn elaborate_term(&mut self, term: ast::Term) -> Result<core::Term, Error> {
        debug!("elaborate_term: term={:?}", term);

        match term {
            ast::Term::Literal { span, lit } => Ok(core::Term::Literal {
                span: span,
                lit: self.elaborate_literal(lit),
            }),
            ast::Term::Var { name, .. } => Ok(core::Term::Var {
                name: try!(self.elaborate_name(name)),
            }),
            ast::Term::Match { .. } => {
                panic!("match elaboration is currently disabled")
            }
            ast::Term::App { fun, arg, span } => {
                let efun = try!(self.elaborate_term(*fun));
                let earg = try!(self.elaborate_term(*arg));

                Ok(core::Term::App {
                    span: span,
                    fun: Box::new(efun),
                    arg: Box::new(earg),
                })
            },
            ast::Term::Forall { name, ty, term, span } =>
                self.enter_scope(vec![(name.clone(), *ty)], move |lcx, locals| {
                    let term = try!(lcx.elaborate_term(*term));
                    Ok(core::Term::abstract_pi(locals, term))
                }),
            ast::Term::Metavar { .. } => panic!("can't elaborate meta-variables"),
            ast::Term::Lambda { args, body, span, .. } => {
                self.enter_scope(args, move |lcx, locals| {
                    let ebody = try!(lcx.elaborate_term(*body));
                    Ok(core::Term::abstract_lambda(locals, ebody))
                })
            }
            ast::Term::Type => Ok(core::Term::Type),
        }
    }

    fn elaborate_literal(&self, lit: ast::Literal) -> core::Literal {
        match lit {
            ast::Literal::Unit => core::Literal::Unit,
            ast::Literal::Int(i) => core::Literal::Int(i),
        }
    }

    fn elaborate_name(&self, name: ast::Name) -> Result<core::Name, Error> {
        debug!("elaborate_name: name={}", name);

        match self.locals.get(&name) {
            None => match self.cx.globals.get(&name) {
                None => Err(Error::UnknownVariable(name.clone())),
                Some(nn) => Ok(nn.clone()),
            },
            Some(local) => Ok(local.clone())
        }
    }
}
