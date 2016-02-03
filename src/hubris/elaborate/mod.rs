mod util;

use ast::{self, SourceMap};
use core;
use typeck::{self, TyCtxt};
use self::util::to_qualified_name;
use super::error_reporting::{Report, ErrorContext};
use term::{self, Terminal, color, StdoutTerminal, Result as TResult};

use std::io::{self, Write};
use std::collections::{HashMap, HashSet};
use std::path::{Path, PathBuf};

#[derive(Debug)]
pub enum Error {
    UnexpectedQualifiedName,
    UnknownVariable(ast::Name),
    TypeCk(typeck::Error),
    InvalidImport,
    Many(Vec<Error>),
}

impl From<typeck::Error> for Error {
    fn from(err: typeck::Error) -> Error {
        Error::TypeCk(err)
    }
}

impl<O: Write, E: ErrorContext<O>> Report<O, E> for Error {
    fn report(self, cx: &mut E) -> TResult<()> {
        match self {
            Error::TypeCk(ty_ck_err) => {
                ty_ck_err.report(cx)
            }
            Error::UnknownVariable(n) => {
                cx.span_error(n.span,
                    format!("unresolved name `{}`", n))
            }
            e => panic!("need to support better error printing for this {:?}", e),
        }
    }
}

pub struct ElabCx {
    module: ast::Module,

    /// The set of declared constructor names in scope
    /// we need this to differentiate between a name
    /// binding or null-ary constructor in pattern
    /// matching.
    constructors: HashSet<ast::Name>,
    globals: HashMap<ast::Name, core::Name>,
    metavar_counter: usize,
    pub ty_cx: TyCtxt,
}

impl ErrorContext<io::Stdout> for ElabCx {
    fn get_source_map(&self) -> &SourceMap {
        &self.ty_cx.source_map
    }

    fn get_terminal(&mut self) -> &mut Box<Terminal<Output=io::Stdout> + Send> {
        &mut self.ty_cx.terminal
    }
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
            metavar_counter: 0,
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
                }
                &ast::Item::Import(ref n) => imports.push(try!(ecx.elaborate_import(n.clone()))),
                _ => {}
            }


            match ecx.elaborate_def(def) {
                Err(e) => errors.push(e),
                Ok(edef) => match edef {
                    None => {},
                    Some(edef) => {
                        match &edef {
                            &core::Item::Data(ref d) => try!(ecx.ty_cx.declare_datatype(d)),
                            &core::Item::Fn(ref f) => ecx.ty_cx.declare_def(f),
                            &core::Item::Extern(ref e) => ecx.ty_cx.declare_extern(e),
                        }

                        defs.push(edef);
                    }
                }
            }
        }

        if errors.len() != 0 {
            Err(Error::Many(errors))
        } else {
            let module = core::Module {
                file_name: path.as_ref().to_owned(),
                name: name,
                defs: defs,
                imports: imports,
            };

            try!(ecx.ty_cx.type_check_module(&module));

            Ok(module)
        }
    }

    pub fn elaborate_import(&mut self, name: ast::Name) -> Result<core::Name, Error> {
        let core_name = to_qualified_name(name);
        let main_file = PathBuf::from(self.ty_cx.source_map.file_name.clone());
        let load_path = main_file.parent().unwrap();
        try!(self.ty_cx.load_import(load_path, &core_name));
        Ok(core_name)
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
            ast::Item::Import(_) => Ok(None),
        }
    }

    fn elaborate_data(&mut self, data: ast::Data) -> Result<core::Data, Error> {
        let ast_rec_name = data.name.in_scope("rec".to_string()).unwrap();
        let ty_name = try!(self.elaborate_global_name(data.name));

        // Pre-declare the recursor name for the time being.
        self.globals.insert(
            ast_rec_name,
            ty_name.in_scope("rec".to_string()).unwrap());

        let mut lcx = LocalElabCx::from_elab_cx(self);

        let data_ctors = data.ctors;
        let data_ty = data.ty;

        let (ctors, ty, params) = try!(lcx.enter_scope(data.parameters.clone(),
        move |lcx, params| {
            let mut ctors = Vec::new();
            for ctor in data_ctors.into_iter() {
                let ector = try!(lcx.elaborate_ctor(&params, ctor));
                ctors.push(ector);
            }

            let ty = core::Term::abstract_pi(
                params.clone(),
                try!(lcx.elaborate_term(data_ty)));

            Ok((ctors, ty, params))
        }));

        Ok(core::Data {
            span: data.span,
            name: ty_name,
            parameters: params,
            ty: ty,
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
            term: try!(LocalElabCx::from_elab_cx(self).elaborate_term(term)),
        })
    }

    fn elaborate_global_name(&mut self, n: ast::Name) -> Result<core::Name, Error> {
        match n.repr.clone() {
            ast::NameKind::Qualified(_) => Err(Error::UnexpectedQualifiedName),
            ast::NameKind::Unqualified(name) => {
                let qn = core::Name::Qual {
                    span: n.span,
                    components: vec![name],
                };

                self.globals.insert(n.clone(), qn.clone());

                Ok(qn)
            }
        }
    }

    fn meta(&mut self, ty: core::Term) -> Result<core::Name, Error> {
        let meta_no = self.metavar_counter;
        let meta = core::Name::Meta {
            number: meta_no,
            ty: Box::new(ty),
        };
        self.metavar_counter += 1;
        Ok(meta)
    }
}

pub struct LocalElabCx<'ecx> {
    cx: &'ecx mut ElabCx,
    locals: HashMap<ast::Name, core::Name>,
}

impl<'ecx> LocalElabCx<'ecx> {
    pub fn from_elab_cx(ecx: &'ecx mut ElabCx) -> LocalElabCx<'ecx> {
        LocalElabCx {
            cx: ecx,
            locals: HashMap::new(),
        }
    }

    fn enter_scope<F, R>(&mut self,
                         binders: Vec<(ast::Name, ast::Term)>,
                         body: F)
                         -> Result<R, Error>
        where F: FnOnce(&mut LocalElabCx, Vec<core::Name>) -> Result<R, Error>
    {
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

    fn elaborate_ctor(&mut self,
                      parameters: &Vec<core::Name>,
                      ctor: ast::Constructor)
                      -> Result<(core::Name, core::Term), Error> {

        let ename = try!(self.cx.elaborate_global_name(ctor.0));

        let ety = try!(self.elaborate_term(ctor.1));
        let ety = core::Term::abstract_pi(parameters.clone(), ety);

        Ok((ename, ety))
    }

    pub fn elaborate_term(&mut self, term: ast::Term) -> Result<core::Term, Error> {
        debug!("elaborate_term: term={:?}", term);

        match term {
            ast::Term::Literal { span, lit } => {
                Ok(core::Term::Literal {
                    span: span,
                    lit: self.elaborate_literal(lit),
                })
            }
            ast::Term::Var { name, .. } => {
                Ok(core::Term::Var { name: try!(self.elaborate_name(name)) })
            }
            ast::Term::Match { .. } => panic!("match elaboration is currently disabled"),
            ast::Term::App { fun, arg, span } => {
                let efun = try!(self.elaborate_term(*fun));
                let earg = try!(self.elaborate_term(*arg));

                Ok(core::Term::App {
                    span: span,
                    fun: Box::new(efun),
                    arg: Box::new(earg),
                })
            }
            ast::Term::Forall { name, ty, term, .. } => {
                self.enter_scope(vec![(name.clone(), *ty)], move |lcx, locals| {
                    let term = try!(lcx.elaborate_term(*term));
                    Ok(core::Term::abstract_pi(locals, term))
                })
            }
            ast::Term::Metavar { .. } => panic!("can't elaborate meta-variables"),
            ast::Term::Lambda { args, body, .. } => {
                self.enter_scope(args, move |lcx, locals| {
                    let ebody = try!(lcx.elaborate_term(*body));
                    Ok(core::Term::abstract_lambda(locals, ebody))
                })
            }
            ast::Term::Let { .. } => panic!("can't elaborate let-bindings"),
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

        // It is most likely to be a local
        match self.locals.get(&name) {
            // A global in the current module
            None => {
                match self.cx.globals.get(&name) {
                    // If it isn't a global we are going to see if the name has already been
                    // loading into the type context, if not this is an error.
                    None => {
                        let core_name = to_qualified_name(name.clone());
                        if self.cx.ty_cx.in_scope(&core_name) {
                            Ok(core_name)
                        } else {
                            Err(Error::UnknownVariable(name.clone()))
                        }
                    }
                    Some(nn) => Ok(nn.clone()),
                }
            }
            Some(local) => Ok(local.clone()),
        }
    }
}
