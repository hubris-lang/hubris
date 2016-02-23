use super::TyCtxt;
use super::constraint::*;
use core::{Term, Name};

use std::collections::{BinaryHeap, HashMap};

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
    constraint_mapping: HashMap<Name, Vec<Constraint>>,
    pub solution_mapping: HashMap<Name, (Term, Justification)>,
    choice_stack: Vec<Choice>,
}

#[derive(Debug, Clone)]
pub enum Error {
    SolverErr
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
                    let simple_cs = solver.simplify(t.clone(), u.clone(), j.clone());
                    for sc in simple_cs {
                        solver.visit(sc);
                    }
                },
                &Constraint::Choice(..) => {
                    solver.visit(c.clone().categorize())
                }
            }
        }
        Ok(solver)
    }

    pub fn visit(&mut self, c: CategorizedConstraint) {
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

    pub fn visit_unification(&mut self, r: Term, s: Term, j: Justification, category: ConstraintCategory) {
        // Find if either term is stuck, if so store the meta-variable
        let meta = match r.is_stuck() {
            Some(m) => m,
            None => match s.is_stuck() {
                None => panic!("one of these should be stuck otherwise the constraint should be gone already I think?"),
                Some(m) => m,
            }
        };

        // See if we have a solution in the solution map,
        // if we have a solution for ?m we should substitute
        // it in both terms and reconstruct the equality
        // constraint.
        //
        // Finally we need to visit every constraint that
        // results.
        if let Some((t, j_m)) = self.solution_for(&meta) {
            let simp_c = self.simplify(
                r.instantiate_meta(&meta, &t),
                s.instantiate_meta(&meta, &t),
                j.join(j_m));

            for sc in simp_c {
                self.visit(sc);
            }
        // If the constraint is a pattern constraint
        //
        } else if category == ConstraintCategory::Pattern {
            // left or right?
            let meta = match r.head().unwrap() {
                Term::Var { name } => name,
                _ => panic!("mis-idetnfied pattern constraint")
            };

            let locals = r.args().unwrap();
            println!("meta {}", meta);

            let locals: Vec<_> =
                locals.into_iter().map(|l|
                match l {
                    Term::Var { ref name } => name.clone(),
                    _ => panic!("mis-idetnfied pattern constraint")
                }).collect();

            let solution = Term::abstract_lambda(locals, s);

            self.solution_mapping.insert(meta, (solution, j));
            // else if the constraint is a pattern h?m ℓ ≈ t, ji then
            // add the assignment ?m 7→ h(abstractλ ℓ t), ji to S
            // for each c in U[?m], visit (c)
        } else {
            panic!()
            // self.add_constraint_for(meta, )
            // else update U, and insert constraint into Q
        }
    }

    pub fn simplify(&self, t: Term, u: Term, j: Justification) -> Vec<CategorizedConstraint> {
        // Case 1: t and u are precisely the same term
        // unification constraints of this form incur
        // no constraints since this is discharge-able here.
        if t == u {
            return vec![];
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
        else if t.head_is_local() && u.head_is_local() {
                panic!()
        }

        else if t.head_is_global() &&
                u.head_is_global() &&
                t.head() == u.head() {
                    // if t.args.meta_free() && u.args.meta_free() {
                    //      self.simplify(t.unfold(f) = u.unfold(f))
                    // } else if !f.reducible() {
                    //     t.args = u.args
                    // } else { panic!() }
            panic!()
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
                     let mut arg_cs = self.simplify(*ty1, *ty2, j.clone());

                     let t_sub = term1.instantiate(&local);
                     let u_sub = term2.instantiate(&local);

                     let body_cs = self.simplify(t_sub, u_sub, j.clone());
                     arg_cs.extend(body_cs.into_iter());

                     arg_cs
                 }
                 _ => panic!("this should be impossible")
            }
        } else {
            if t.is_stuck().is_some() ||
               u.is_stuck().is_some() {
                vec![Constraint::Unification(t, u, j).categorize()]
            } else {
                panic!("use judgement to report error {:?}", j)
            }
        }
    }

    // The set of constraints should probably just be a lazy list.
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
                Constraint::Unification(..) => {
                    panic!("{:?}", c)
                }
            }
        }


        Ok(self.solution_mapping)
    }

    pub fn resolve(&self, just: Justification) -> Result<(), Error> {
        panic!("{:?}", just);
    }
}
