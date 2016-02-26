mod constraint;
mod error;
mod inductive;
mod solver;

use core::*;
use super::ast::{SourceMap, ModuleId, Span, HasSpan};
use super::parser;
use super::session::{HasSession, Session, Reportable};
use super::elaborate::{self};
pub use self::error::Error;
use self::constraint::*;
use term::{Terminal, stdout, StdoutTerminal};

use std::cell::RefCell;
use std::collections::HashMap;
use std::io::{self};
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
    types: HashMap<Name, Data>,
    functions: HashMap<Name, Function>,

    pub axioms: HashMap<Name, Term>,
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

    pub fn load_import(&mut self, path: &Path, name: &Name) -> Result<(), Error> {
        debug!("load_import: path={} module={}", path.display(), name);
        let file_suffix = match name_to_path(name) {
            None => panic!(),
            Some(f) => f,
        };

        let file_to_load = path.join(file_suffix);
        debug!("load_import: file_to_load={}", file_to_load.display());

        let id = self.session.next_module_id();
        let parser = try!(parser::from_file(&file_to_load, id));
        let module = try!(parser.parse());

        self.session.add_source_map_for(id, parser.source_map);

        let mut ecx = elaborate::ElabCx::from_module(
            module,
            self.session.clone());

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
        self.axioms.insert(data_type.name.clone(), data_type.ty.clone());

        // Each constructor also becomes a constant with type ascribed
        // in its definition.
        for ctor in &data_type.ctors {
            let name = ctor.0.clone();
            let ty = ctor.1.clone();
            self.axioms.insert(name, ty);
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
        self.axioms.insert(e.name.clone(), e.term.clone());
    }

    pub fn declare_axiom(&mut self, e: &Axiom) {
        self.axioms.insert(e.name.clone(), e.ty.clone());
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
                    None => Err(Error::UnknownVariable(name.clone())),
                    Some(t) => Ok(t),
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
        Ok((term.clone(), vec![]))
    }

    pub fn eval(&self, term: &Term) -> Result<Term, Error> {
        use core::Term::*;

        debug!("eval: {}", term);

        let result = match term {
            &App { ref fun, ref arg, span } => {
                let efun = try!(self.eval(fun));
                // This is call by value
                let earg = try!(self.eval(arg));

                match efun {
                    Term::Lambda { ref body, .. } => {
                        self.eval(&body.instantiate(&earg))
                    }
                    f => Ok(App {
                        fun: Box::new(f),
                        arg: Box::new(earg),
                        span: span,
                    })
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
            &Term::Var { ref name } => self.unfold_name(name),
            &Term::Recursor(ref ty_name, ref premises, ref scrutinee) => {

                debug!("ty_name: {}", ty_name);
                let scrutinee = try!(self.eval(scrutinee));
                debug!("scrutinee: {}", scrutinee);
                match self.types.get(&ty_name) {
                    None => panic!("type checking bug: can not find inductive type {}", ty_name),
                    Some(dt) => {
                        for (i, ctor) in dt.ctors.iter().enumerate() {
                            let name = &ctor.0;
                            let ctor_ty = &ctor.1;
                            match scrutinee.head() {
                                None => panic!("arg to recursor must be in (w)hnf"),
                                Some(head) => {
                                    if name.to_term() == head {
                                        let premise = premises[i].clone();

                                        let is_recursive =
                                            self.is_recursive_ctor(ty_name, ctor_ty);

                                        if !is_recursive {
                                            return Ok(premise);
                                        } else {
                                            let args: Vec<_> =
                                                scrutinee.args()
                                                         .unwrap();

                                            // Need to skip the parameters
                                            let args =
                                                args.iter()
                                                    .skip(dt.parameters.len());

                                            let tys =
                                                premise.binders()
                                                       .unwrap();

                                            // debug!("premise: {}", premise);
                                            // debug!("scurtinee: {}", scrutinee);

                                            let mut term_args = vec![];
                                            let mut recursor_args = vec![];

                                            for (arg, ty) in args.zip(tys.into_iter()) {
                                                // debug!("arg : {}", arg);
                                                // println!("ty : {}", ty);
                                                if ty.head().unwrap() == ty_name.to_term() {
                                                    let rec =
                                                    Recursor(
                                                        ty_name.clone(),
                                                        premises.clone(),
                                                        Box::new(arg.clone()));
                                                    recursor_args.push(rec);
                                                }

                                                term_args.push(arg.clone());
                                            }

                                            let mut args = term_args;
                                            args.extend(recursor_args.into_iter());

                                            return self.eval(&Term::apply_all(premise.clone(), args));
                                        }
                                    }
                                }
                            }
                        }
                        panic!("this shouldn't happen")
                    }
                }
            }
            t => Ok(t.clone()),
        };

        debug!("eval result {:?}", result);

        result
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

    pub fn type_check_term(
            &mut self,
            term: &Term,
            expected_ty: Option<Term>) -> Result<(Term, Term), Error> {
        debug!("type_check_term: term={}", term);

        let (infer_ty, mut infer_cs) = try!(self.type_infer_term(term));

        match &expected_ty {
            &None => {}
            &Some(ref ty) => {
                infer_cs.push(
                    Constraint::Unification(
                        infer_ty.clone(),
                        ty.clone(),
                        Justification::Asserted));
            }
        }

        let mut solver =
            try!(solver::Solver::new(self, infer_cs));

        let solutions = try!(solver.solve());

        for sol in &solutions {
            println!("{}", (sol.1).0);
        }

        // Finally use the solutions given to us by the solver or
        // throw an error if there is not a solution for a meta-var
        // occurring in them
        let new_term = try!(replace_metavars(term.clone(), &solutions));

        Ok((new_term, expected_ty.unwrap_or(infer_ty)))
    }

    pub fn ensure_sort(&self, term: Term) -> CkResult {
        if term.is_sort() {
            return Ok(constrain(term, vec![]));
        } else {
            panic!()
        }
    }

    pub fn ensure_forall(&self, term: Term, sp: Span) -> CkResult {
        if term.is_forall() {
            return Ok(constrain(term, vec![]));
        }

        let (tp, cs) = try!(self.whnf(&term));

        if tp.is_forall() {
            Ok((tp, cs))
        } else if let Some(m) = tp.is_stuck() {
            panic!()
        } else {
            Err(Error::ExpectedFunction(sp, term))
        }
    }

    pub fn type_infer_term(&mut self, term: &Term) -> CkResult {
        match term {
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

                match pi_type {
                    Term::Forall { binder, term, .. } => {
                        let (arg_ty, arg_cs) =
                            try!(self.type_infer_term(arg));
                        let term = try!(self.eval(&term.instantiate(arg)));
                        constraints.extend(arg_cs.into_iter());
                        constraints.push(
                            Constraint::Unification(
                                arg_ty,
                                *binder.ty,
                                Justification::Asserted));
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
                let name = &binder.name;
                let ty = &binder.ty;

                let mut constraints = vec![];
                let local = self.local(binder.clone());
                let term = term.instantiate(&local.to_term());

                let (sort, ty_cs) = try!(self.type_infer_term(&*ty));
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
                let name = &binder.name;
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
            _ => panic!(),
        }
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
            def_eq_name_modulo(name1, name2, constraints)
        }
        (&Recursor(ref name1, ref premises1, ref scrut1),
         &Recursor(ref name2, ref premises2, ref scrut2)) => {
            if def_eq_name_modulo(name1, name2, constraints) {
                // Should we check premises1.len() == premises2.len()?
                premises1.iter().zip(premises2.iter()).all(|(p1, p2)|
                    def_eq_modulo(p1, p2, constraints));

                def_eq_modulo(scrut1, scrut2, constraints)
            } else {
                false
            }
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

fn def_eq_name_modulo(
    n1: &Name,
    n2: &Name,
    constraints: &mut ConstraintSeq) -> bool {

    debug!("equal_name_modulo: {} == {}", n1, n2);

    match (n1, n2) {
        (&Name::Meta { .. }, &Name::Meta { .. }) => {
            panic!()
        }
        (&Name::Meta { .. }, n) => {
            panic!()
        }
        (n, &Name::Meta { .. }) => {
            panic!()
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

            path.set_extension("hb");

            Some(path)
        }
        _ => None,
    }
}

pub fn replace_metavars(t: Term, subst_map: &HashMap<Name, (Term, Justification)>) -> Result<Term, Error> {
    use core::Term::*;

    match t {
        App { fun, arg, span } => {
            Ok(App {
                fun: Box::new(try!(replace_metavars(*fun, subst_map))),
                arg: Box::new(try!(replace_metavars(*arg, subst_map))),
                span: span,
            })
        }
        Forall { binder, term, span } => {
            Ok(Forall {
                binder: try!(subst_meta_binder(binder, subst_map)),
                term: Box::new(try!(replace_metavars(*term, subst_map))),
                span: span,
            })
        }
        Lambda { binder, body, span } => {
            Ok(Lambda {
                binder: try!(subst_meta_binder(binder, subst_map)),
                body: Box::new(try!(replace_metavars(*body, subst_map))),
                span: span,
            })
        }
        Var { ref name } if name.is_meta() => {
            match subst_map.get(&name) {
                None => panic!("no solution found for {}", name),
                Some(x) => Ok(x.clone().0)
            }

        }
        v @ Var { .. } => Ok(v),
        l @ Literal { .. } => Ok(l),
        Type => Ok(Type),
        Recursor(..) => panic!(),
    }
}

pub fn subst_meta_binder(
        mut b: Binder,
        subst_map: &HashMap<Name, (Term, Justification)>) -> Result<Binder, Error> {
    b.ty = Box::new(try!(replace_metavars(*b.ty, subst_map)));
    Ok(b)
}
