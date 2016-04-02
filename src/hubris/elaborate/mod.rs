mod pattern_matching;
mod util;

use ast::{self, HasSpan};
use core;
use typeck::{self, TyCtxt};
use session::{HasSession, Session, Reportable};
use self::util::to_qualified_name;
use self::pattern_matching::elaborate_pattern_match;

use std::io::{self};
use std::collections::{HashMap, HashSet};

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

impl Reportable for Error {
    fn report(self, session: &Session) -> io::Result<()> {
        match self {
            Error::TypeCk(ty_ck_err) => {
                ty_ck_err.report(session)
            }
            Error::UnknownVariable(n) => {
                session.span_error(n.span,
                    format!("unresolved name `{}`", n))
            }
            Error::Many(es) => {
                for e in es {
                    try!(e.report(session))
                }

                Ok(())
            }
            e => panic!("need to support better error printing for this {:?}", e),
        }
    }
}

pub struct ElabCx {
    /// The current module being elaborated.
    module: ast::Module,
    /// The set of declared constructor names in scope
    /// we need this to differentiate between a name
    /// binding or null-ary constructor in pattern
    /// matching.
    constructors: HashSet<ast::Name>,
    /// Set of globally translated names.
    globals: HashMap<ast::Name, core::Name>,
    /// A global counter for metavariable numbers, this should probably
    /// be thread specific.
    metavar_counter: usize,
    /// Elaboration relies on type checking, the type checker produces
    /// an "inferred" type containing meta variables along with a set
    /// of constraints that must be solved, in order for type checking
    /// to be complete.
    pub ty_cx: TyCtxt,
}

impl HasSession for ElabCx {
    fn session(&self) -> &Session {
        &self.ty_cx.session
    }
}

impl ElabCx {
    pub fn from_module(module: ast::Module, session: Session) -> ElabCx {
        // Elaboration relies on type checking, so first we setup a typing context to use
        // while elaborating the program. We will run the type checker in inference mode
        // to setup constraints, and then we will solve the constraints, and check the
        // term again.
        let mut ty_cx = TyCtxt::empty();
        ty_cx.session = session;

        ElabCx {
            module: module,
            constructors: HashSet::new(),
            globals: HashMap::new(),
            metavar_counter: 0,
            ty_cx: ty_cx,
        }
    }

    pub fn elaborate_module(&mut self) -> Result<core::Module, Error> {
        let module_name = self.module.name.clone();

        let name = try!(self.elaborate_global_name(module_name));

        let mut errors = vec![];
        let mut defs = vec![];
        let mut imports = vec![];

        for def in self.module.items.clone().into_iter() {
            match &def {
                &ast::Item::Inductive(ref d) => {
                    for ctor in &d.ctors {
                        self.constructors.insert(ctor.0.clone());
                    }
                }
                &ast::Item::Import(ref n) =>
                    imports.push(try!(self.elaborate_import(n.clone()))),
                _ => {}
            }


            match self.elaborate_def(def) {
                Err(e) => errors.push(e),
                Ok(edef) => match edef {
                    None => {},
                    Some(edef) => {
                        defs.push(edef);
                    }
                }
            }
        }

        if errors.len() > 0 {
            return Err(Error::Many(errors))
        } else {
            let module = core::Module {
                file_name: self.ty_cx.session.root_file().to_owned(),
                name: name,
                defs: defs,
                imports: imports,
            };

            try!(self.ty_cx.type_check_module(&module));

            Ok(module)
        }
    }

    pub fn elaborate_import(&mut self, name: ast::Name) -> Result<core::Name, Error> {
        let core_name = to_qualified_name(name).unwrap();
        try!(self.ty_cx.load_import(&core_name));
        Ok(core_name)
    }

    pub fn elaborate_def(&mut self, def: ast::Item) -> Result<Option<core::Item>, Error> {
        debug!("elaborate_def: def={:?}", def);

        match def {
            ast::Item::Inductive(d) => {
                let edata = try!(self.elaborate_data(d));
                try!(self.ty_cx.declare_datatype(&edata));
                Ok(Some(core::Item::Data(edata)))
            }
            ast::Item::Def(def) => {
                let edef = try!(self.elaborate_fn(def));
                try!(self.ty_cx.declare_def(&edef));
                debug!("elaborate_def: def={}", edef);
                Ok(Some(core::Item::Fn(edef)))
            }
            ast::Item::Axiom(ax) => {
                let eax = try!(self.elaborate_axiom(ax));
                self.ty_cx.declare_axiom(&eax);
                Ok(Some(core::Item::Axiom(eax)))
            }
            ast::Item::Extern(e) => {
                let ext = core::Item::Extern(try!(self.elaborate_extern(e)));
                Ok(Some(ext))
            }
            ast::Item::Comment(_) |
            ast::Item::Import(_) => Ok(None),
        }
    }

    fn elaborate_data(&mut self, data: ast::Inductive) -> Result<core::Data, Error> {
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
        |lcx, params| {
            let ty = core::Term::abstract_pi(
                params.clone(),
                try!(lcx.elaborate_term(data_ty)));

            // TODO: Fix this shouldn't expose so many details,
            // but the elaborator has to interleave with the type
            // checker better.
            lcx.cx.ty_cx.axioms.insert(ty_name.clone(), typeck::Axiom::new(ty.clone()));

            let mut ctors = Vec::new();
            for ctor in data_ctors.into_iter() {
                let ector = try!(lcx.elaborate_ctor(&params, ctor));
                ctors.push(ector);
            }

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

    fn elaborate_fn(&mut self, fun: ast::Def) -> Result<core::Function, Error> {
        let mut lcx = LocalElabCx::from_elab_cx(self);

        lcx.enter_scope(fun.args.clone(), move |lcx, args| {
            let name = try!(lcx.cx.elaborate_global_name(fun.name));
            let ty = try!(lcx.elaborate_term(fun.ty.clone()));
            let ebody = try!(lcx.elaborate_term(fun.body));

            debug!("elaborate_fn: ty={} body={}", ty, ebody);

            let body = core::Term::abstract_lambda(args.clone(), ebody);
            let ret_ty = core::Term::abstract_pi(args.clone(), ty);
            // Clear up the whole elaboration vs. decl appraochh,
            // not happy with it right now.
            let (body, ret_ty) = try!(lcx.cx.ty_cx.type_check_term(&body, Some(ret_ty)));

            Ok(core::Function {
                name: name,
                args: args.clone(),
                // We compute the full type of the function here
                // by taking the return type and abstracting
                // over the arguments.
                ret_ty: ret_ty,
                // We construct a lambda representing the body
                // with all of the function's parameters abstracted.
                body: body,
            })
        })
    }

    fn elaborate_axiom(&mut self, ax: ast::Axiom) -> Result<core::Axiom, Error> {
        let ast::Axiom { span, name, ty } = ax;
        Ok(core::Axiom {
            span: span,
            name: try!(self.elaborate_global_name(name)),
            ty: try!(LocalElabCx::from_elab_cx(self).elaborate_term(ty)),
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

    pub fn elaborate_global_name(&mut self, n: ast::Name) -> Result<core::Name, Error> {
        match n.repr.clone() {
            ast::NameKind::Qualified(components) => {
                let qn = core::Name::Qual {
                    span: n.span,
                    components: components,
                };

                self.globals.insert(n.clone(), qn.clone());

                Ok(qn)
            }
            ast::NameKind::Unqualified(name) => {
                let qn = core::Name::Qual {
                    span: n.span,
                    components: vec![name],
                };

                self.globals.insert(n.clone(), qn.clone());

                Ok(qn)
            }
            ast::NameKind::Placeholder => Err(Error::UnexpectedQualifiedName),
        }
    }
}

pub struct LocalElabCx<'ecx> {
    cx: &'ecx mut ElabCx,
    locals: HashMap<ast::Name, core::Name>,
    // This is kind of a shitty hack to keep the HashMap above ordered, should probably
    // write a utility data strcture.
    locals_in_order: Vec<core::Name>,
}

impl<'ecx> LocalElabCx<'ecx> {
    pub fn from_elab_cx(ecx: &'ecx mut ElabCx) -> LocalElabCx<'ecx> {
        LocalElabCx {
            cx: ecx,
            locals: HashMap::new(),
            locals_in_order: Vec::new(),
        }
    }

    fn enter_scope<F, R>(&mut self,
                         binders: Vec<ast::Binder>,
                         body: F)
                         -> Result<R, Error>
        where F: FnOnce(&mut LocalElabCx, Vec<core::Name>) -> Result<R, Error>
    {
        let mut locals = vec![];

        let old_context = self.locals.clone();
        let old_locals_in_order = self.locals_in_order.clone();

        // A binder can contain multiple names like so:
        // (A B C : T) will result in a binder with
        // 3 names to bind, so we then do an inner
        // loop.

        for binder in binders {
            let binder_ty = binder.ty;
            for name in binder.names.into_iter().rev() {
                let repr = match name.clone().repr {
                    ast::NameKind::Qualified(..) => panic!(),
                    ast::NameKind::Unqualified(s) => s,
                    ast::NameKind::Placeholder => "_".to_string(),
                };

                let eterm = try!(self.elaborate_term(binder_ty.clone().unwrap()));

                let binding_info = match binder.mode {
                    ast::BindingMode::Implicit => core::BindingMode::Implicit,
                    ast::BindingMode::Explicit => core::BindingMode::Explicit,
                };

                let local = self.cx.ty_cx.local_with_repr_and_mode(repr, eterm, binding_info);

                self.locals.insert(name, local.clone());
                self.locals_in_order.push(local.clone());
                locals.push(local);
            }
        }

        let result = try!(body(self, locals));

        // Restore the previous context.
        self.locals = old_context;
        self.locals_in_order = old_locals_in_order;

        Ok(result)
    }

    fn elaborate_ctor(&mut self,
                      parameters: &Vec<core::Name>,
                      ctor: ast::Constructor)
                      -> Result<(core::Name, core::Term), Error> {

        let ename = try!(self.cx.elaborate_global_name(ctor.0));

        let ety = try!(self.elaborate_term(ctor.1));
        // TODO: Need to figure out if params are implicit for this ctor or not
        let ety = core::Term::abstract_pi_implicit(parameters.clone(), ety);

        Ok((ename, ety))
    }

    pub fn apply_implicit_args(&mut self, term: core::Term) -> Result<core::Term, Error> {
        let mut fun_ty =
            try!(self.cx.ty_cx.type_infer_term(&term)).0;

        let mut result = term;

        while let core::Term::Forall { binder, term, .. } = fun_ty {
            if binder.is_implicit() {
                let implicit_arg =
                    try!(self.implicit_argument(*binder.ty));
                // It is important any time we do an application to simulate it
                // at the type level by instantiating the body of the type,
                // if not this results in constraints that are not subst.
                // correctly.
                fun_ty = term.instantiate(&implicit_arg);
                result = core::Term::apply(result, implicit_arg);
            } else {
                break;
            }
        }

        Ok(result)
    }

    pub fn elaborate_term(&mut self, term: ast::Term) -> Result<core::Term, Error> {
        debug!("elaborate_term: term={:?}", term);

        match term {
            ast::Term::Literal { span, lit } => {
                panic!()
            }
            ast::Term::Var { name, .. } => {
                self.elaborate_name(name)
            }
            ast::Term::Match { scrutinee, cases, span } => {
                elaborate_pattern_match(self, *scrutinee, cases)
            }
            app @ ast::Term::App { .. } => {
                let span = app.get_span();
                let (head, args) = app.uncurry();

                let implicit = match &head {
                    &ast::Term::Var { implicit, .. } => implicit,
                    _ => true,
                };

                let efun = try!(self.elaborate_term(head));

                let mut eargs = vec![];

                for arg in args {
                    let earg = try!(self.elaborate_term(arg));
                    eargs.push(try!(self.apply_implicit_args(earg)));
                }

                let efun = if implicit {
                    try!(self.apply_implicit_args(efun))
                } else {
                    efun
                };

                let mut app = core::Term::apply_all(efun, eargs);

                app.set_span(span);

                Ok(app)
            }
            ast::Term::Forall { binders, term, .. } => {
                self.enter_scope(binders, move |lcx, locals| {
                    let term = try!(lcx.elaborate_term(*term));
                    Ok(core::Term::abstract_pi(locals, term))
                })
            }
            ast::Term::Lambda { args, body, .. } => {
                self.enter_scope(args, move |lcx, locals| {
                    let ebody = try!(lcx.elaborate_term(*body));
                    Ok(core::Term::abstract_lambda(locals, ebody))
                })
            }
            ast::Term::Let { bindings, body, span } => {
                // // Currently we elaborate let expressions by
                // // constructing a lambda in which we bind each
                // // name that occurs in the let binding, then
                // // apply it to the terms bound to the names
                // // in sequence.
                // let mut binders = vec![];
                // let mut terms = vec![];
                // for (n, ty, term) in bindings {
                //     binders.push((n, ty));
                //     terms.push(term);
                // }
                //
                // self.enter_scope(binders, move |lcx, locals| {
                //     let ebody = try!(lcx.elaborate_term(*body));
                //     let lambda = core::Term::abstract_lambda(locals, ebody);
                //     Ok(core::Term::apply_all(lambda, terms))
                // })
                panic!("let bindings can not be elaborated")
            },
            ast::Term::Type => Ok(core::Term::Type),
        }
    }

    fn elaborate_literal(&self, lit: ast::Literal) -> core::Term {
        panic!()
    }

    fn elaborate_name(&mut self, name: ast::Name) -> Result<core::Term, Error> {
        debug!("elaborate_name: name={}", name);

        // Wish we had seme regions
        let placeholder = match name.repr {
            ast::NameKind::Placeholder => Some(try!(self.make_placeholder())),
            _ => None,
        };

        // It is most likely to be a local
        let mut core_name = match self.locals.get(&name) {
            // A global in the current module
            None => {
                match self.cx.globals.get(&name) {
                    // If it isn't a global we are going to see if the name has already been
                    // loading into the type context, if not this is an error.
                    None => {
                        match to_qualified_name(name.clone()) {
                            None => placeholder.unwrap(),
                            Some(ref core_name) if self.cx.ty_cx.in_scope(core_name) => {
                                core_name.to_term()
                            }
                            Some(_) => {
                                return Err(Error::UnknownVariable(name.clone()))
                            }
                        }
                    }
                    Some(nn) => nn.to_term(),
                }
            }
            Some(local) => local.to_term(),
        };

        // IMPORTANT!: Make sure we update the span here for the precise name being elaborated
        // we store how we choose to translate the name in the tables, but this results in
        // us using the first occurence of `name` s span everywhere for name.
        core_name.set_span(name.span);

        Ok(core_name)
    }

    fn implicit_argument(&mut self, ty: core::Term) -> Result<core::Term, Error> {
        self.meta_in_context(ty)
    }

    fn make_placeholder(&mut self) -> Result<core::Term, Error> {
        let meta_no = self.cx.metavar_counter;

        let meta_ty = core::Name::Meta {
            number: meta_no,
            ty: Box::new(core::Term::Type),
        };

        self.cx.metavar_counter += 1;

        self.meta_in_context(meta_ty.to_term())
    }

    fn meta_in_context(&mut self, ty: core::Term) -> Result<core::Term, Error> {
        let meta_no = self.cx.metavar_counter;

        let ty =
            core::Term::abstract_pi(self.locals_in_order.clone(), ty);

        let args: Vec<_> =
            self.locals_in_order
                .iter()
                .map(core::Name::to_term)
                .collect();

        let meta = core::Name::Meta {
            number: meta_no,
            ty: Box::new(ty),
        };

        self.cx.metavar_counter += 1;

        Ok(core::Term::apply_all(meta.to_term(), args))
    }
}
