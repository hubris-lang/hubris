use hubris_syntax::ast::HasSpan;
use super::TyCtxt;
use super::constraint::*;
use super::super::session::{HasSession, Session, Reportable};
use core::{Term, Name};

use std::collections::{BinaryHeap, HashMap};
use std::io;

pub struct Choice {
    constraints: BinaryHeap<CategorizedConstraint>,
    constraint_mapping: HashMap<Name, Vec<CategorizedConstraint>>,
    solution_mapping: HashMap<Name, (Term, Justification)>,
    assumption_justification: Justification,
    constraint_justification: Justification,
}

pub struct Solver<'tcx> {
    ty_cx: &'tcx mut TyCtxt,
    constraints: BinaryHeap<CategorizedConstraint>,
    constraint_mapping: HashMap<Name, Vec<CategorizedConstraint>>,
    pub solution_mapping: HashMap<Name, (Term, Justification)>,
    choice_stack: Vec<Choice>,
}

#[derive(Debug, Clone)]
pub enum Error {
    Justification(Justification),
}

impl Reportable for Error {
    fn report(self, cx: &Session) -> io::Result<()> {
        match self {
            Error::Justification(j) => match j {
                Justification::Asserted(by) => match by {
                    AssertedBy::Application(u, t) =>
                        cx.span_error(u.get_span(), format!("applied {} to {}", u, t)),
                    AssertedBy::ExpectedFound(infer_ty, ty) =>
                        cx.span_error(ty.get_span(),
                            format!("expected type `{}` found `{}`", ty, infer_ty)),
                },
                Justification::Assumption => cx.error("assumption".to_string()),
                Justification::Join(r1, sr2) => cx.error("assumption".to_string()),
            },
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
            match &c {
                &Constraint::Unification(ref t, ref u, ref j) => {
                    let simple_cs =
                        try!(solver.simplify(t.clone(), u.clone(), j.clone()));
                    for sc in simple_cs {
                        solver.visit(sc);
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

    pub fn visit_unification(&mut self, r: Term, s: Term, j: Justification, category: ConstraintCategory) -> Result<(), Error> {
        println!("visit_unification: r={} s={}", r, s);

        for (m, sol) in &self.solution_mapping {
            println!("solution: {}={}", m, sol.0);
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
            println!("r: {} u: {}", r, s);
            // left or right?
            let meta = match r.head().unwrap_or_else(|| s.head().unwrap()) {
                Term::Var { name } => name,
                _ => panic!("mis-idetnfied pattern constraint")
            };

            let locals = r.args().unwrap_or(vec![]);

            println!("meta {}", meta);

            let locals: Vec<_> =
                locals.into_iter().map(|l|
                match l {
                    Term::Var { ref name } => name.clone(),
                    _ => panic!("mis-idetnfied pattern constraint")
                }).collect();

            let solution = Term::abstract_lambda(locals, s);

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
            println!("category: {:?}", category);

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
        println!("t: {} u: {}", t, u);
        // Case 1: t and u are precisely the same term
        // unification constraints of this form incur
        // no constraints since this is discharge-able here.
        if t == u {
            return Ok(vec![]);
        }

        // Case 2: if t can beta/iota reduce to then
        // we reduce t ==> t' and create a constraint
        // between t' and u (t' = u).
        else if t.is_bi_reducible() {
            panic!();
        }

        // Case 3: if the head of t and u are constants
        // we should generate constraints between each of their
        // arguments for example l s_1 .. s_n = l t_1 .. t_n
        // creates (s_1 = t_1, j) ... (s_n = t_n, j).
        else if t.head_is_local() && u.head_is_local() && t.head() == u.head() {
            println!("t={} u={}", t, u);
            let t_args = t.args().unwrap().into_iter();
            let u_args = u.args().unwrap().into_iter();

            let mut cs = vec![];
            for (s, r) in t_args.zip(u_args) {
                let arg_cs = try!(self.simplify(s, r, j.clone()));
                cs.extend(arg_cs.into_iter())
            }

            Ok(cs)
        }

        else if t.is_app() &&
                u.is_app() &&
                t.head_is_global() &&
                u.head_is_global() &&
                t.head() == u.head() {

            let f = t.head().unwrap();

            let t_args_meta_free =
                t.args().map(|args|
                    args.iter().all(|a| !a.is_meta())).unwrap_or(false);

            let u_args_meta_free =
                u.args().map(|args|
                    args.iter().all(|a| !a.is_meta())).unwrap_or(false);

            if f.is_bi_reducible() &&
               t_args_meta_free &&
               u_args_meta_free {
                panic!("var are free")
                    //      self.simplify(t.unfold(f) = u.unfold(f))
                    // } else if !f.reducible() {
                    //     t.args = u.args
                    // } else { panic!() }
            } else if !f.is_bi_reducible() {
                Ok(t.args().unwrap()
                 .into_iter()
                 .zip(u.args().unwrap().into_iter())
                 .map(|(t_i, s_i)| Constraint::Unification(t_i, s_i, j.clone()).categorize())
                 .collect())
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
            match (t, u) {
                (Term::Forall { binder: binder1, term: term1, .. },
                 Term::Forall { binder: binder2, term: term2, .. }) => {
                     let name1 = binder1.name.clone();
                     let name2 = binder2.name;

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
                Err(Error::Justification(j))
            }
        }
    }

    // The set of constraints should probably be a lazy list.
    fn process(&self, cs: Vec<CategorizedConstraint>, j: Justification) {
        // for c in &self.constraints {
        //     println!("{:?}", c);
        //     match c.constraint {
        //         Constraint::Choice(..) => panic!("can't process choice constraints"),
        //         Constraint::Unification(..) => {
        //             match c.category {
        //
        //             }
        //         }
        //
        //     }
        // }
        // assert!(self.constraints.len() > 0);
    }

    pub fn solve(mut self) -> Result<HashMap<Name, (Term, Justification)>, Error> {
        while let Some(c) = self.constraints.pop() {
            println!("{:?}", c);
            match c.constraint {
                Constraint::Choice(..) => panic!("can't process choice constraints"),
                Constraint::Unification(t, u, j) => {
                    for (m, s) in &self.solution_mapping {
                        println!("{} {}", m, s.0)
                    }
                    match c.category {
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
                                println!("t {} u {}", t_head, u_head);
                            } else {
                                panic!("flex-flex solution is not eq")
                            }
                        }
                        cat => panic!("solver can't handle {:?} {} = {} by {:?}", cat, t, u, j)
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
