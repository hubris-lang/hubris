use hubris_syntax::ast::HasSpan;
use super::TyCtxt;
use super::constraint::*;
use super::super::session::{HasSession, Session, Reportable};
use core::{Term, Binder, Name};
use util::*;

use std::collections::{BinaryHeap, HashMap};
use std::io;
use std::rc::Rc;

pub struct Choice {
    constraints: BinaryHeap<CategorizedConstraint>,
    constraint_mapping: HashMap<Name, Vec<CategorizedConstraint>>,
    solution_mapping: HashMap<Name, (Term, Justification)>,
    assumption_justification: Justification,
    constraint_justification: Justification,
    list: (),
}

pub struct Solver<'tcx> {
    ty_cx: &'tcx mut TyCtxt,
    constraints: BinaryHeap<CategorizedConstraint>,
    constraint_mapping: HashMap<Name, Vec<CategorizedConstraint>>,
    pub solution_mapping: HashMap<Name, (Term, Justification)>,
    choice_stack: Vec<Choice>,
}

#[derive(Debug)]
pub enum Error {
    Simplification(Justification),
    Justification(Justification),
    TypeCk(Box<super::Error>),
    NoSolution(Vec<Name>, Term),
    Many(Vec<Error>),
}

impl From<super::Error> for Error {
    fn from(err: super::Error) -> Error {
        Error::TypeCk(Box::new(err))
    }
}

impl Reportable for Error {
    fn report(self, cx: &Session) -> io::Result<()> {
        match self {
            Error::Justification(j) => match j {
                Justification::Asserted(by) => match by {
                    AssertedBy::Application(span, u, t) =>
                        cx.span_error(span,
                            format!("a term with type `{}` can not be applied to an argument with \
                                     type `{}`", u, t)),
                    AssertedBy::ExpectedFound(infer_ty, ty) =>
                        cx.span_error(ty.get_span(),
                            format!("expected type `{}` found `{}`", ty, infer_ty)),
                },
                Justification::Assumption => cx.error("assumption".to_string()),
                j @ Justification::Join(_, _) => panic!(), // cx.error(format!("{}", j)),
            },
            Error::NoSolution(ns, term) => {
                // TODO: fix this
                cx.error(format!("unable to find a solution for {} in {}", ns[0], term))
            }
            Error::Many(errs) => {
                for err in errs {
                    try!(err.report(cx));
                }

                Ok(())
            }
            _ => panic!()
        }
    }
}

impl<'tcx> Solver<'tcx> {
    fn empty(ty_cx: &'tcx mut TyCtxt) -> Solver<'tcx> {
        Solver {
            ty_cx: ty_cx,
            constraints: BinaryHeap::new(),
            constraint_mapping: HashMap::new(),
            solution_mapping: HashMap::new(),
            choice_stack: vec![],
        }
    }

    /// Take a typing context and a sequence of constraints, and setup an
    /// instance of the solver.
    pub fn new(ty_cx: &'tcx mut TyCtxt, cs: ConstraintSeq) -> Result<Solver, Error> {
        let mut solver = Solver::empty(ty_cx);
        for c in cs {
            // debug!("Solver::new: c={}", c);
            match &c {
                &Constraint::Unification(ref t, ref u, ref j) => {
                    let simple_cs =
                        try!(solver.simplify(t.clone(), u.clone(), j.clone()));
                    for sc in simple_cs {
                        try!(solver.visit(sc));
                    }
                },
                &Constraint::Choice(..) => {
                    try!(solver.visit(c.clone().categorize()))
                }
            }
        }
        Ok(solver)
    }

    pub fn visit(&mut self, c: CategorizedConstraint) -> Result<(), Error> {
        let CategorizedConstraint {
            category,
            constraint,
        } = c;

        match constraint {
            Constraint::Unification(t, u, j) =>
                self.visit_unification(t, u, j, category),
            Constraint::Choice(..) =>
                panic!("choice constraints aren't impl"),
        }
    }

    pub fn solution_for(&self, name: &Name) -> Option<(Term, Justification)> {
        self.solution_mapping.get(name).map(|x| x.clone())
    }

    pub fn add_solution(&mut self, name: Name, solution: (Term, Justification)) {
        self.solution_mapping.insert(name, solution);
    }

    pub fn visit_unification(&mut self, r: Term, s: Term, j: Justification, category: ConstraintCategory) -> Result<(), Error> {
        debug!("visit_unification: r={} s={}", r, s);

        for (m, sol) in &self.solution_mapping {
            debug!("solution: {}={}", m, sol.0);
        }

        // Find the correct meta-variable to solve for,
        // either.
        let meta = match (r.is_stuck(), s.is_stuck()) {
            (Some(m1), Some(m2)) => {
                if self.solution_for(&m1).is_some() {
                    m1
                } else {
                    m2
                }
            }
            (Some(m), None) | (None, Some(m)) => {
                m
            }
            _ => panic!("one of these should be stuck otherwise the constraint should be gone already I think?"),
        };

        debug!("meta {}", meta);
        // See if we have a solution in the solution map,
        // if we have a solution for ?m we should substitute
        // it in both terms and reconstruct the equality
        // constraint.
        //
        // Finally we need to visit every constraint that
        // results.
        if let Some((t, j_m)) = self.solution_for(&meta) {
            let simp_c = try!(self.simplify(
                r.instantiate_meta(&meta, &t),
                s.instantiate_meta(&meta, &t),
                j.join(j_m)));

            for sc in simp_c {
                try!(self.visit(sc));
            }

            Ok(())
        } else if category == ConstraintCategory::Pattern {
            debug!("r: {} u: {}", r, s);

            let (meta, locals) = r.uncurry();

            let meta = match meta {
                Term::Var { name } => name,
                _ => panic!(),
            };

            assert!(meta.is_meta());
            debug!("meta {}", meta);
            // There is a case here I'm not sure about
            // what if the meta variable we solve has been
            // also applied to non-local constants?

            // Currently we just filter map, and don't
            // abstract over those.
            let locals: Result<Vec<_>, Error> =
                locals.into_iter().map(|l|
                match l {
                    Term::Var { ref name } if (name.is_local()) => Ok(name.clone()),
                    Term::Var { ref name } if name.is_qual() => {
                        let ty= try!(self.ty_cx.type_infer_term(&name.to_term()));
                        Ok(self.ty_cx.local_with_repr("_".to_string(), ty.0))
                    }
                    _ => panic!("mis-identified pattern constraint")
                }).collect();

            // TI breaks here, this is dumb.
            let locals = try!(locals);

            for local in &locals {
                debug!("local={:?}", local);
            }

            debug!("rhs: {}", s);

            let solution = Term::abstract_lambda(locals, s);

            debug!("soultion: {} ", solution);

            assert!(meta.is_meta());

            self.solution_mapping.insert(meta.clone(), (solution, j));

            let cs = match self.constraint_mapping.get(&meta) {
                None => vec![],
                Some(cs) => cs.clone(),
            };

            for c in cs {
                try!(self.visit(c.clone()));
            }

            Ok(())
        } else {
            debug!("category: {:?}", category);

            let cat_constraint = CategorizedConstraint {
                category: category,
                constraint: Constraint::Unification(r, s, j),
            };

            let mut cs = match self.constraint_mapping.remove(&meta) {
                None => vec![],
                Some(cs) => cs,
            };

            cs.push(cat_constraint.clone());

            self.constraint_mapping.insert(meta, cs);
            self.constraints.push(cat_constraint);

            Ok(())
        }
    }

    pub fn simplify(&self, t: Term, u: Term, j: Justification) -> Result<Vec<CategorizedConstraint>, Error> {
        debug!("simplify: t={} u={}", t, u);
        // Case 1: t and u are precisely the same term
        // unification constraints of this form incur
        // no more constraints since this is discharge-able here.
        if t == u {
            debug!("simplify: equal case");
            // debug!("exactly equal");
            return Ok(vec![]);
        }

        // Case 2: if t can beta/iota reduce to then
        // we reduce t ==> t' and create a constraint
        // between t' and u (t' = u).
        else if self.ty_cx.is_bi_reducible(&t) &&
                self.ty_cx.is_bi_reducible(&u) {
            debug!("simplify: reduce case (both)");
            self.simplify(try!(self.ty_cx.eval(&t)),
                          try!(self.ty_cx.eval(&u)), j)
        } else if self.ty_cx.is_bi_reducible(&t) {
            debug!("simplify: reduce case (t)");
            self.simplify(try!(self.ty_cx.eval(&t)), u, j)
        } else if self.ty_cx.is_bi_reducible(&u) {
            debug!("simplify: reduce case (u)");
            self.simplify(t, try!(self.ty_cx.eval(&u)), j)
        }

        // Case 3: if the head of t and u are constants
        // we should generate constraints between each of their
        // arguments for example l s_1 .. s_n = l t_1 .. t_n
        // creates (s_1 = t_1, j) ... (s_n = t_n, j).
        else if t.head_is_local() && u.head_is_local() && t.head() == u.head() {
            debug!("inside local head t={} u={}", t, u);
            let t_args = t.args().unwrap().into_iter();
            let u_args = u.args().unwrap().into_iter();

            let mut cs = vec![];
            for (s, r) in t_args.zip(u_args) {
                let arg_cs = try!(self.simplify(s, r, j.clone()));
                cs.extend(arg_cs.into_iter())
            }

            Ok(cs)
        }

        else if t.head_is_global() &&
                u.head_is_global() &&
                t.head() == u.head() {
            debug!("head is global");

            let (f, f_args) = t.uncurry();
            let (g, g_args) = u.uncurry();

            if f_args.len() == 0 && g_args.len() == 0 {
                panic!()
            }

            // THERE IS A BUG HERE!
            let t_args_meta_free =
                f_args.iter().all(|a| !a.is_meta());

            let u_args_meta_free =
                g_args.iter().all(|a| !a.is_meta());

            if (self.ty_cx.is_bi_reducible(&t) ||
                self.ty_cx.is_bi_reducible(&u))  &&
               t_args_meta_free &&
               u_args_meta_free {
                panic!("var are free")
                    //      self.simplify(t.unfold(f) = u.unfold(f))
                    // } else if !f.reducible() {
                    //     t.args = u.args
                    // } else { panic!() }
            } else if !self.ty_cx.is_bi_reducible(&f) && f == g {
                let mut cs = vec![];
                for (t_i, s_i) in f_args.into_iter().zip(g_args.into_iter()) {
                    debug!("arg_equal {} {}", t_i, s_i);
                    cs.extend(try!(self.simplify(t_i, s_i, j.clone())).into_iter());
                }
                Ok(cs)
            } else {
                panic!("f is reducible but metavars are ")
            }
        }

        // This should be the case dealing with depth, haven't implemented it
        // yet.
        else if false {
            panic!()
        }

        // else if t.is_lambda() && u.is_lambda() {
        //     panic!()
        // }

        else if t.is_forall() && u.is_forall() {
            debug!("inside forall");
            match (t, u) {
                (Term::Forall { binder: binder1, term: term1, .. },
                 Term::Forall { binder: binder2, term: term2, .. }) => {
                     let ty1 = binder1.ty.clone();
                     let ty2 = binder2.ty;

                     let local = self.ty_cx.local(binder1).to_term();
                     let mut arg_cs = try!(self.simplify(*ty1, *ty2, j.clone()));

                     let t_sub = term1.instantiate(&local);
                     let u_sub = term2.instantiate(&local);

                     let body_cs = try!(self.simplify(t_sub, u_sub, j.clone()));
                     arg_cs.extend(body_cs.into_iter());

                     Ok(arg_cs)
                 }
                 _ => panic!("this should be impossible")
            }
        } else {
            if t.is_stuck().is_some() ||
               u.is_stuck().is_some() {
                Ok(vec![Constraint::Unification(t, u, j).categorize()])
            } else {
                let j = try!(self.eval_justification(j));
                panic!("{} {}", t, u);
                Err(Error::Justification(j))
            }
        }
    }

    /// Will take the justification that was created at constraint generation time, and substitute
    /// all known meta-variable solutions and then simplify it. This is particularly useful in
    /// error reporting where we want to show the simplest term possible.
    fn eval_justification(&self, j: Justification) -> Result<Justification, Error> {
        use super::constraint::Justification::*;
        // Since we are already doing error reporting, we will just use this to accumulate
        // the meta-vars that can't be subst, and do nothing with them.
        let mut errs = vec![];
        let j = match j {
            Asserted(by) => Asserted(match by {
                AssertedBy::Application(span, t, u) => {
                    let t = try!(self.ty_cx.eval(&replace_metavars_with_err(t, &self.solution_mapping, &mut errs)));
                    let u = try!(self.ty_cx.eval(&replace_metavars_with_err(u, &self.solution_mapping, &mut errs)));

                    AssertedBy::Application(span, t, u)
                }
                AssertedBy::ExpectedFound(t, u) => {
                    let t = try!(self.ty_cx.eval(&replace_metavars_with_err(t, &self.solution_mapping, &mut errs)));
                    let u = try!(self.ty_cx.eval(&replace_metavars_with_err(u, &self.solution_mapping, &mut errs)));

                    AssertedBy::ExpectedFound(t, u)
                }
            }),
            Assumption => Assumption,
            Join(j1, j2) => {
                let j1 = try!(self.eval_justification((&*j1).clone()));
                let j2 = try!(self.eval_justification((&*j2).clone()));
                Join(Rc::new(j1), Rc::new(j2))
            }
        };
        Ok(j)
    }

    // The set of constraints should probably be a lazy list.
    fn process(&self, cs: &mut Iterator<Item=CategorizedConstraint>, j: Justification) -> Result<(), Error> {
        match cs.next() {
            None => self.resolve(j),
            Some(c) => panic!(),
        }
    }

    // /// Create a case split (choice point) by storing the current solver state with
    // /// the arguments provided.
    // fn case_split(&mut self, j_a: Justification, j_c: Justification, list: () ) {
    //
    // }
    pub fn solve(mut self) -> Result<HashMap<Name, (Term, Justification)>, Error> {
        while let Some(c) = self.constraints.pop() {
            debug!("Solver::solve: constraint={}", c.constraint);
            match c.constraint {
                Constraint::Choice(term, ty, f, j) =>
                    // self.process(f(term, ty, subst), j)
                    panic!(),
                Constraint::Unification(t, u, j) => {
                    for (m, s) in &self.solution_mapping {
                        debug!("{} {}", m, s.0)
                    }
                    match c.category {
                        ConstraintCategory::Delta => {
                            panic!("can't handle delta constraints yet")
                        }
                        ConstraintCategory::QuasiPattern |
                        ConstraintCategory::FlexRigid => {
                            let (t_head, t_args) = t.uncurry();
                            let (u_head, u_args) = u.uncurry();

                            let term = u_head;

                            let bound_vars = t_args.clone();

                            for arg in u_args {
                                debug!("{}", arg)
                            }


                            debug!("t_head {}", t_head);
                            let infer_ty = try!(self.ty_cx.type_infer_term(&t_head));

                            let t_head = match t_head {
                                Term::Var { name } => name,
                                _ => panic!()
                            };

                            let locals =
                                infer_ty.0.binders()
                                          .unwrap()
                                          .into_iter()
                                          .map(|t| self.ty_cx.local_with_repr("x".to_string(), t.clone()))
                                          .collect();

                            let solution = Term::abstract_lambda(locals, term);
                            debug!("infer_ty {}", infer_ty.0);

                            for arg in bound_vars {
                                debug!("to_bind: {}", arg);
                            }

                            debug!("sol {}; {} = {}", solution, t_head, u);
                            self.add_solution(t_head, (solution, j));
                        }
                        ConstraintCategory::FlexFlex => {
                            // Need to clean this code up
                            let t_head = match t.head().unwrap() {
                                Term::Var { name , .. } => name,
                                _ => panic!()
                            };

                            let u_head = match t.head().unwrap() {
                                Term::Var { name , .. } => name,
                                _ => panic!()
                            };

                            if self.solution_for(&t_head) == self.solution_for(&u_head) {
                                debug!("t {} u {}", t_head, u_head);
                            } else {
                                panic!("flex-flex solution is not eq")
                            }
                        }
                        ConstraintCategory::Pattern => {
                            panic!("solver failure pattern constraints should never reach here")
                        }
                        ConstraintCategory::Recursor => {
                            panic!("can't handle recursor constraints yet")
                        }
                        ConstraintCategory::OnDemand  => {
                            panic!("can't handle on demand constraints yet")
                        }
                        ConstraintCategory::Ready |
                        ConstraintCategory::Regular |
                        ConstraintCategory::Postponed => {
                            panic!("unification constraints should never be one of these")
                        }
                    }
                }
            }
        }
        Ok(self.solution_mapping)
    }

    pub fn resolve(&self, just: Justification) -> Result<(), Error> {
        panic!("{:?}", just);
    }
}

pub fn replace_metavars(
        term: Term,
        subst_map: &HashMap<Name, (Term, Justification)>) -> Result<Term, Error> {
    let mut errs = vec![];
    let term = replace_metavars_with_err(term, subst_map, &mut errs);

    if errs.len() > 0 {
        Err(Error::NoSolution(errs, term))
    } else {
        Ok(term)
    }
}

pub fn replace_metavars_with_err(
        t: Term, subst_map: &HashMap<Name, (Term, Justification)>,
        errs: &mut Vec<Name>) -> Term {
    use core::Term::*;

    match t {
        App { fun, arg, span } => {
            App {
                fun: Box::new(replace_metavars_with_err(*fun, subst_map, errs)),
                arg: Box::new(replace_metavars_with_err(*arg, subst_map, errs)),
                span: span,
            }
        }
        Forall { binder, term, span } => {
            Forall {
                binder: subst_meta_binder(binder, subst_map, errs),
                term: Box::new((replace_metavars_with_err(*term, subst_map, errs))),
                span: span,
            }
        }
        Lambda { binder, body, span } => {
            Lambda {
                binder: subst_meta_binder(binder, subst_map, errs),
                body: Box::new(replace_metavars_with_err(*body, subst_map, errs)),
                span: span,
            }
        }
        Var { ref name } if name.is_meta() => {
            match subst_map.get(&name) {
                None => {
                    errs.push(name.clone());
                    name.to_term()
                },
                // Not an effcient approach, should normalize the map up-front
                Some(x) => replace_metavars_with_err(x.clone().0, subst_map, errs)
            }

        }
        v @ Var { .. } => v,
        Type => Type,
    }
}

pub fn subst_meta_binder(
        mut b: Binder,
        subst_map: &HashMap<Name, (Term, Justification)>,
        errs: &mut Vec<Name>) -> Binder {
    b.ty = Box::new(replace_metavars_with_err(*b.ty, subst_map, errs));
    b
}
