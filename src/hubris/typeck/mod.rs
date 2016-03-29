mod constraint;
mod error;
mod inductive;
mod solver;

use core::{
    self, Name,
    Term, Binder, Item, Function, Data,
    Module, Extern, BindingMode};
use super::ast::{Span, HasSpan};
use super::parser;
use super::session::{HasSession, Session, Reportable};
use super::elaborate::{self};
pub use self::error::Error;
use self::constraint::*;
use self::solver::replace_metavars;
use term::{stdout, StdoutTerminal};

use std::cell::RefCell;
use std::collections::HashMap;
use std::path::{PathBuf, Path};

pub enum DeltaReduction {
    Reducible,
    Semireducible,
    Irreducible,
}

/// A definition
pub struct Definition {
    ty: Term,
    term: Term,
    reduction: DeltaReduction,
}

pub type ComputationRule = Box<Fn(&TyCtxt, Term) -> Result<Term, Error>>;

/// An axiom
pub struct Axiom {
    pub ty: Term,
    /// Adds a computation rule to the axiom
    pub computation_rule: Option<ComputationRule>,
}

impl Axiom {
    pub fn new(ty: Term) -> Axiom {
        Axiom {
            ty: ty,
            computation_rule: None,
        }
    }
}

impl Definition {
    fn new(ty: Term, term: Term) -> Definition {
        Definition {
            ty: ty,
            term: term,
            reduction: DeltaReduction::Reducible,
        }
    }
}
/// A global context for type checking containing the necessary information
/// needed across type checking all definitions.
pub struct TyCtxt {
    // We keep these around right now, but I'm not sure if we should.
    pub types: HashMap<Name, Data>,
    functions: HashMap<Name, Function>,

    pub axioms: HashMap<Name, Axiom>,
    definitions: HashMap<Name, Definition>,

    pub session: Session,

    local_counter: RefCell<usize>,
    pub terminal: Box<StdoutTerminal>,
}

pub type CkResult = Result<(Term, ConstraintSeq), Error>;

impl TyCtxt {
    pub fn empty() -> TyCtxt {
        TyCtxt {
            types: HashMap::new(),
            functions: HashMap::new(),
            axioms: HashMap::new(),
            definitions: HashMap::new(),
            session: Session::empty(),
            local_counter: RefCell::new(0),
            terminal: stdout().unwrap(),
        }
    }

    pub fn from_module(module: &Module, session: Session) -> Result<TyCtxt, Error> {
        let mut ty_cx = TyCtxt::empty();
        ty_cx.session = session;

        try!(ty_cx.type_check_module(module));

        Ok(ty_cx)
    }

    pub fn type_check_module(&mut self, module: &Module) -> Result<(), Error> {
        // let main_file = PathBuf::from(self.source_map.file_name.clone());
        // let prefix = main_file.parent().unwrap();

        // Should be idempotent, is currently not.
        // for import in &module.imports {
        //     try!(self.load_import(&prefix, import));
        // }

        for def in &module.defs {
            match def {
                &Item::Data(ref d) => try!(self.declare_datatype(d)),
                &Item::Fn(ref f) => try!(self.declare_def(f)),
                &Item::Extern(ref e) => self.declare_extern(e),
                &Item::Axiom(ref ax) => self.declare_axiom(ax),
            }

            try!(self.type_check_def(def));
        }


        Ok(())
    }

    pub fn in_scope(&self, name: &Name) -> bool {
        self.axioms.contains_key(name) || self.definitions.contains_key(name)
    }

    pub fn load_import(&mut self, name: &Name) -> Result<(), Error> {
        debug!("load_import: module_name={}", name);
        let file_suffix = match name_to_path(name) {
            None => panic!(),
            Some(f) => f,
        };

        let file_to_load = self.session.resolve_path(&file_suffix);

        self.load_import_from_path(&file_to_load)
    }

    pub fn load_import_from_path(&mut self, file_to_load: &Path) -> Result<(), Error> {
        debug!("load_import_from_path: file_to_load={}", file_to_load.display());

        if !self.session.is_loaded(&file_to_load) {
            let id = self.session.next_module_id();
            let parser = try!(parser::from_file(&file_to_load, id));
            let module = try!(parser.parse());

            // Add a source map for error reporting
            self.session.add_source_map_for(id, parser.source_map);

            // Construct a new elaboration context for this module.
            let mut ecx = elaborate::ElabCx::from_module(
                module,
                self.session.clone());

            // Elaborate the module
            let emodule =
                ecx.elaborate_module();

            // Should find a way to gracefully exit, or report error and continue function
            match emodule {
                Err(e) => {
                    try!(ecx.report(e));
                    // We should return an import error here
                    Ok(())
                },
                Ok(emodule) => {
                    let ty_cx = try!(TyCtxt::from_module(&emodule, self.session.clone()));
                    self.merge(ty_cx)
                }
            }
        } else {
            Ok(())
        }
    }

    pub fn merge(&mut self, ty_cx: TyCtxt) -> Result<(), Error> {
        let TyCtxt {
            types,
            functions,
            axioms,
            definitions,
            ..
        } = ty_cx;

        let mut errors = vec![];

        for (n, ty) in types {
            if let Some(_) = self.types.insert(n.clone(), ty) {
                errors.push(Error::NameExists(n))
            }
        }

        for (n, fun) in functions {
            if let Some(_) = self.functions.insert(n.clone(), fun) {
                errors.push(Error::NameExists(n))
            }
        }

        for (n, axiom) in axioms {
            if let Some(_) = self.axioms.insert(n.clone(), axiom) {
                errors.push(Error::NameExists(n))
            }

        }

        for (n, def) in definitions {
            if let Some(_) = self.definitions.insert(n.clone(), def) {
                errors.push(Error::NameExists(n));
            }
        }

        if errors.len() != 0 {
            Err(Error::Many(errors))
        } else {
            Ok(())
        }
    }

    pub fn get_main_body(&self) -> Result<&Term, Error> {
        match self.functions.get(&Name::from_str("main")) {
            None => Err(Error::NoMain),
            Some(ref f) => Ok(&f.body),
        }
    }

    pub fn declare_datatype(&mut self, data_type: &Data) -> Result<(), Error> {
        // Currently we use types/functions for metadata, do we need them?
        self.types.insert(data_type.name.clone(), data_type.clone());

        // The type is just a constant with the type `ty`
        self.axioms.insert(data_type.name.clone(), Axiom::new(data_type.ty.clone()));

        // Each constructor also becomes a constant with type ascribed
        // in its definition.
        for ctor in &data_type.ctors {
            let name = ctor.0.clone();
            let ty = ctor.1.clone();
            let axiom = Axiom::new(ty);
            self.axioms.insert(name, axiom);
        }

        inductive::make_recursor(self, data_type)
    }

    pub fn declare_def(&mut self, f: &Function) -> Result<(), Error> {
        self.functions.insert(f.name.clone(), f.clone());
        let (term, ty) = try!(self.type_check_term(&f.body, Some(f.ret_ty.clone())));
        let def = Definition::new(ty, term);
        self.definitions.insert(f.name.clone(), def);

        Ok(())
    }

    /// Declaring an external function creates an axiom in the type checker
    /// with the appropriate type.
    ///
    /// During code generation we will deal with creating a symbol for this
    /// function.
    pub fn declare_extern(&mut self, e: &Extern) {
        let axiom = Axiom::new(e.term.clone());
        self.axioms.insert(e.name.clone(), axiom);
    }

    pub fn declare_axiom(&mut self, e: &core::Axiom) {
        let axiom = Axiom::new(e.ty.clone());
        self.axioms.insert(e.name.clone(), axiom);
    }

    pub fn type_check_def(&mut self, def: &Item) -> Result<(), Error> {
        debug!("type_check_def: def={}", def);
        match def {
            &Item::Fn(ref fun) => {
                let &Function {
                    ref ret_ty,
                    ref body, ..
                } = fun;

                try!(self.type_check_term(&body, Some(ret_ty.clone())));
                Ok(())
            }
            _ => Ok(()),
        }
    }

    pub fn lookup_global(&self, name: &Name) -> Result<&Term, Error> {
        match self.definitions.get(name) {
            None => {
                match self.axioms.get(name) {
                    None => {
                        for (ref n, _) in &self.definitions {
                            println!("name: {}", n);
                        }
                        Err(Error::UnknownVariable(name.clone()))
                    }
                    Some(t) => Ok(&t.ty),
                }
            }
            Some(t) => Ok(&t.ty),
        }
    }

    pub fn local(&self, binder: Binder) -> Name {
        let repr = match &binder.name {
            &Name::DeBruijn { ref repr, .. } => repr,
            _ => panic!("creating local {:?}", binder.name),
        };

        self.local_with_repr_and_mode(repr.clone(), *binder.ty, binder.mode)
    }

    pub fn local_with_repr(&self, repr: String, ty: Term) -> Name {
        let new_local = Name::Local {
            number: *self.local_counter.borrow(),
            ty: Box::new(ty),
            repr: repr.clone(),
            binding_info: BindingMode::Explicit,
        };

        *self.local_counter.borrow_mut() += 1;

        new_local
    }

    pub fn local_with_repr_and_mode(&self, repr: String, ty: Term, mode: BindingMode) -> Name {
        let new_local = Name::Local {
            number: *self.local_counter.borrow(),
            ty: Box::new(ty),
            repr: repr.clone(),
            binding_info: mode,
        };

        *self.local_counter.borrow_mut() += 1;

        new_local
    }

    /// Will try to unfold a name if it is unfoldable
    pub fn unfold_name(&self, n: &Name) -> Result<Term, Error> {
        use core::Name::*;

        match n {
            q @ &Qual { .. } => {
                // TODO: also check axioms and report an error about unfolding axioms
                // TODO: we actually need to know whether a name is Opaque or not
                // Or we can't implement this
                match self.definitions.get(q) {
                    None => Ok(n.to_term()), // panic!("failed to lookup name {}", q),
                    Some(t) => Ok(t.term.clone()),
                }
            }
            &DeBruijn { .. } |
            &Meta { .. } |
            &Local { .. } => Ok(n.to_term()),
        }
    }

    pub fn unfold(&self, mut t: Term, n: &Name) -> Result<Term, Error> {
        let def_rhs = try!(self.unfold_name(n));
        let nt = n.to_term();

        t.replace_term(&def_rhs,
                       &|term| self.def_eq(Span::dummy(), term, &nt).is_err());

        Ok(t)
    }

    /// Checks whether a constructor's type is recursive
    pub fn is_recursive_ctor(&self, ty_name: &Name, mut ctor_ty: &Term) -> bool {
        let mut is_rec = false;

        while let &Term::Forall { ref binder, ref term, .. } = ctor_ty {
            match binder.ty.head() {
                None => is_rec = is_rec || false,
                Some(head) =>
                    if head == ty_name.to_term() {
                        return true;
                    }
            }
            ctor_ty = term;
        }

        return is_rec;
    }

    pub fn whnf(&self, term: &Term) -> CkResult {
        debug!("whnf: {}", term);
        match term {
            &Term::App { ref fun, ref arg, span } => {
                let efun = try!(self.whnf(fun)).0;
                // This is call by value
                let earg = try!(self.whnf(arg)).0;

                match efun {
                    Term::Lambda { ref body, .. } => {
                        self.whnf(&body.instantiate(&earg))
                    }
                    f => Ok((Term::App {
                        fun: Box::new(f),
                        arg: Box::new(earg),
                        span: span,
                    }, vec![]))
                }
            }
            _ => Ok((term.clone(), vec![]))
        }
    }

    pub fn eval(&self, term: &Term) -> Result<Term, Error> {
        use core::Term::*;

        debug!("eval: {}", term);

        let result = match term {
            app @ &App { .. } => {
                debug!("eval (apply): app={}", app);
                let span = app.get_span();
                let (head, args) = app.uncurry();

                let efun = try!(self.eval(&head));

                let mut eargs = vec![];
                for arg in args {
                    eargs.push(try!(self.eval(&arg)));
                }

                match efun {
                    lambda @ Term::Lambda { .. } => {
                        let mut lambda = lambda;
                        for earg in eargs {
                            match lambda {
                                Term::Lambda { body, .. } => {
                                    lambda = body.instantiate(&earg);
                                }
                                _ => panic!("evaluation error")
                            }
                        }
                        Ok(try!(self.eval(&lambda)))
                    }
                    Term::Var { ref name } => {
                        if let Some(comp_rule) = self.computation_rule(name) {
                            let t = Term::apply_all(Term::Var { name: name.clone() }, eargs);
                            comp_rule(self, t)
                        } else {
                            let mut t = Term::apply_all(Term::Var { name: name.clone() }, eargs);
                            t.set_span(span);
                            Ok(t)
                        }
                    },
                    t => panic!("type checker bug")
                }
            }
            &Term::Forall { ref binder, ref term, span } => {
                let ety = try!(self.eval(&*binder.ty));
                let eterm = try!(self.eval(term));

                Ok(Forall {
                    binder: Binder::with_mode(binder.name.clone(), ety, binder.mode.clone()),
                    term: Box::new(eterm),
                    span: span,
                })
            }
            &Term::Lambda { ref binder, ref body, span } => {
                let ety = try!(self.eval(&*binder.ty));
                let eterm = try!(self.eval(body));

                Ok(Lambda {
                    binder: Binder::with_mode(binder.name.clone(), ety, binder.mode.clone()),
                    body: Box::new(eterm),
                    span: span,
                })
            }
            &Term::Var { ref name } => self.unfold_name(name),
            &Term::Type => Ok(Term::Type)
        };

        let result = try!(result);

        debug!("eval: result={}", result);

        Ok(result)
    }

    /// Check whether a term is beta/iota reducible.
    pub fn is_bi_reducible(&self, term: &Term) -> bool {
        debug!("is_bi_reducible: term={}", term);
        let (head, args) = term.uncurry();
        if args.len() > 0 {
            !head.is_meta() && !head.is_constant() && term.is_app()
        } else {
            false
        }
    }

    pub fn computation_rule(&self, name: &Name) -> Option<&ComputationRule> {
        self.axioms.get(name).and_then(|x| x.computation_rule.as_ref())
    }

    // TODO: currently this reports that two terms are not equal, we should probably
    // specialize this for type checking saying that for term T, with inferred type
    // U, is not equal to specified type W.
    pub fn def_eq(&self, span: Span, t: &Term, u: &Term) -> CkResult {
        debug!("def_eq: {} {}", t, u);
        let t = try!(self.eval(t));
        let u = try!(self.eval(u));

        let mut constraints = vec![];

        let is_def_eq = def_eq_modulo(
            &t,
            &u,
            &mut constraints);

        if is_def_eq {
            Ok((t.clone(), constraints))
        } else {
            // Should
            Err(Error::DefUnequal(span, t.clone(), u.clone(), vec![]))
        }
    }

    pub fn type_check_term(&mut self,
                           term: &Term,
                           expected_ty: Option<Term>) -> Result<(Term, Term), Error> {
        debug!("type_check_term: term={}", term);

        let (infer_ty, mut infer_cs) = try!(self.type_infer_term(term));

        match &expected_ty {
            &None => {}
            &Some(ref ty) => {
                let just =
                    Justification::Asserted(
                        AssertedBy::ExpectedFound(
                            infer_ty.clone(),
                            ty.clone()));

                debug!("inferred {} expected {}", infer_ty, ty);

                infer_cs.push(
                    Constraint::Unification(
                        infer_ty.clone(),
                        ty.clone(),
                        just));
            }
        }

        let solver = try!(solver::Solver::new(self, infer_cs));

        let solutions = try!(solver.solve());

        for (meta, sol) in &solutions {
            debug!("solutions: meta={} {}", meta, sol.0);
        }

        // Finally use the solutions given to us by the solver or
        // throw an error if there is not a solution for a meta-var
        // occurring in them
        let new_term = try!(replace_metavars(term.clone(), &solutions));

        debug!("term={}\nnew_term={}", term, new_term);

        let infer_ty = try!(replace_metavars(infer_ty.clone(), &solutions));

        Ok((new_term, expected_ty.unwrap_or(infer_ty)))
    }

    pub fn ensure_sort(&self, term: Term) -> CkResult {
        if term.is_sort() {
            return Ok(constrain(term, vec![]));
        } else {
            panic!("ensure sort {}", term);
        }
    }

    pub fn ensure_forall(&self, term: Term, sp: Span) -> CkResult {
        if term.is_forall() {
            return Ok(constrain(term, vec![]));
        }

        let (tp, cs) = try!(self.whnf(&term));

        if tp.is_forall() {
            Ok((tp, cs))
        } else if let Some(_) = tp.is_stuck() {
            panic!()
        } else {
            Err(Error::ExpectedFunction(sp, term))
        }
    }

    pub fn type_infer_term(&mut self, term: &Term) -> CkResult {
        debug!("type_infer_term: term={}", term);
        let result = match term {
            &Term::Var { ref name, .. } => {
                match name {
                    &Name::Local { ref ty, .. } =>
                        Ok(constrain(*ty.clone(), vec![])),
                    q @ &Name::Qual { .. } => {
                        let global_ty = try!(self.lookup_global(q).map(Clone::clone));
                        Ok(constrain(global_ty, vec![]))
                    }
                    &Name::Meta { ref ty, .. } => {
                        Ok(constrain(*ty.clone(), vec![]))
                    }
                    _ => {
                        panic!("internal error: all variable occurences must be free when type \
                                checking a variable")
                    }
                }
            }
            &Term::App { ref fun, ref arg, span } => {
                let mut constraints = vec![];

                let (pi_type, pi_cs) =
                    try!(self.type_infer_term(fun));

                let (pi_type, ensure_cs) =
                    try!(self.ensure_forall(pi_type, fun.get_span()));

                constraints.extend(pi_cs.into_iter());
                constraints.extend(ensure_cs.into_iter());

                match pi_type.clone() {
                    Term::Forall { binder, term, .. } => {
                        let (arg_ty, arg_cs) =
                            try!(self.type_infer_term(arg));

                        let term = term.instantiate(arg);

                        constraints.extend(arg_cs.into_iter());

                        let just =
                            Justification::Asserted(
                                AssertedBy::Application(
                                    fun.get_span(),
                                    pi_type,
                                    arg_ty.clone()));


                        debug!("{} = {}", arg_ty, binder.ty);

                        constraints.push(
                            Constraint::Unification(
                                arg_ty,
                                *binder.ty,
                                just));

                        // TODO: add type checking obliation here
                        Ok(constrain(term, constraints))
                    }
                    t => Err(Error::ApplicationMismatch(
                        span,
                        *fun.clone(),
                        *arg.clone(),
                        t,
                        Term::Type))
                }
            }
            &Term::Forall { ref binder, ref term, .. } => {
                let ty = &binder.ty;

                let mut constraints = vec![];
                let local = self.local(binder.clone());
                let term = term.instantiate(&local.to_term());

                let (sort, ty_cs) = try!(self.type_infer_term(&*ty));
                println!("before forall sort");
                let (_, sort_cs) = try!(self.ensure_sort(sort));
                constraints.extend(ty_cs.into_iter());
                constraints.extend(sort_cs.into_iter());

                let (sort, ty_cs) = try!(self.type_infer_term(&term));
                let (_, sort_cs) = try!(self.ensure_sort(sort));
                constraints.extend(ty_cs.into_iter());
                constraints.extend(sort_cs.into_iter());

                Ok(constrain(Term::Type, constraints))
            }
            &Term::Lambda { ref binder, ref body, span, } => {
                let ty = &binder.ty;

                let mut constraints = vec![];

                let (arg_ty, arg_cs) = try!(self.type_infer_term(&ty));
                let (_, sort_cs) = try!(self.ensure_sort(arg_ty));

                constraints.extend(arg_cs.into_iter());
                constraints.extend(sort_cs.into_iter());

                let local = self.local(binder.clone());
                let body = body.instantiate(&local.to_term());

                let (pi_body, body_cs) =
                    try!(self.type_infer_term(&body));

                constraints.extend(body_cs.into_iter());

                let forall = Term::Forall {
                    span: span,
                    binder: binder.clone(),
                    term: Box::new(pi_body.abstr(&local)),
                };

                Ok(constrain(forall, constraints))
            }
            &Term::Type =>
                Ok(constrain(Term::Type, vec![])),
        };

        let (t, cs) = try!(result);
        debug!("type_infer_term: term={}, infer_ty={}", term, t);
        Ok((t, cs))
    }

    pub fn evaluate(&self, term: &Term) -> Term {
        term.clone()
    }
}

fn def_eq_modulo(
    t1: &Term,
    t2: &Term,
    constraints: &mut ConstraintSeq) -> bool {
    use core::Term::*;

    debug!("equal_modulo: {} == {}", t1, t2);

    match (t1, t2) {
        (&App { fun: ref fun1, arg: ref arg1, .. },
         &App { fun: ref fun2, arg: ref arg2, .. }) => {
            def_eq_modulo(fun1, fun2, constraints) &&
            def_eq_modulo(arg1, arg2, constraints)
        }
        (&Forall { binder: ref binder1, term: ref term1, .. },
         &Forall { binder: ref binder2, term: ref term2, .. }) => {
            def_eq_modulo(&*binder1.ty, &*binder2.ty, constraints) &&
            def_eq_modulo(term1, term2, constraints)
        }
        (&Lambda { binder: ref binder1, body: ref body1, .. },
         &Lambda { binder: ref binder2, body: ref body2, ..}) => {
            def_eq_modulo(&*binder1.ty, &*binder2.ty, constraints) &&
            def_eq_modulo(body1, body2, constraints)
        }
        (&Var { name: ref name1 }, &Var { name: ref name2 }) => {
            def_eq_name_modulo(name1, name2)
        }
        (t, u) => {
            if t.is_stuck().is_some() || u.is_stuck().is_some() {
                // let constraint = Constraint::Unification(
                //     t.clone(),
                //     u.clone(), Justification::Asserted);
                // constraints.push(constraint);
                true
            } else {
                t == u
            }
        }
    }
}

fn def_eq_name_modulo(n1: &Name, n2: &Name) -> bool {
    debug!("equal_name_modulo: {} == {}", n1, n2);

    match (n1, n2) {
        (&Name::Meta { number: number1, .. },
         &Name::Meta { number: number2, .. }) => {
            number1 == number2
        }
        (&Name::Meta { .. }, _) => {
            false
        }
        (_, &Name::Meta { .. }) => {
            false
        }
        _ => n1 == n2
    }
}

fn name_to_path(name: &Name) -> Option<PathBuf> {
    match name {
        &Name::Qual { ref components, .. } => {
            assert!(components.len() > 0);
            let mut cs = components.iter();
            let first = cs.next().unwrap();
            let mut path = PathBuf::from(first);

            for c in cs {
                path = path.join(c);
            }

            path.set_extension("hbr");

            Some(path)
        }
        _ => None,
    }
}

#[test]
fn test_is_bi_reducible() {
    let ty_cx = TyCtxt::new();
    panic!()
}
