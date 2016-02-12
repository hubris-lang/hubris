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
    solution_mapping: HashMap<Name, (Term, Justification)>,
    choice_stack: Vec<Choice>,
}

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

    pub fn new(ty_cx: &'tcx mut TyCtxt, cs: ConstraintSeq) -> Solver {
        let solver = Solver::empty(ty_cx);
        for c in cs {
            solver.visit(c);
        }
    }

    pub fn visit(&self, c: Constraint) {
        match &c {
            &Constraint::Unification(t, u, j) =>
                self.visit_unification(t, u, j),
            &Constraint::Choice(..) =>
                panic!("choice constraints aren't impl"),
        }
    }

    pub fn visit_unification(&mut self, r: Term, s: Term, j: Justification) {
        let r_stuck = r.is_stuck();
        let s_stuck = s.is_stuck();

        if r_stuck.is_meta() || s_stuck.is_meta() {
            let r = r.instantiate_meta(panic!("?m"));
            self.simplify(r, s, j);
        }
// if r or s is stuck by some ?m and ?m 7→ ht, jmi in S then
// visit (simp hr[?m := t] ≈ s[?m := t], j ⊲⊳ jmi)
// else if the constraint is a pattern h?m ℓ ≈ t, ji then
// add the assignment ?m 7→ h(abstractλ ℓ t), ji to S
// for each c in U[?m], visit (c)
// else update U, and insert constraint into Q
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
        else if t.can_bi_reduce() {
            panic!();
        }

        // Case 3: if the head of t and u are constants
        // we should generate constraints between each of their
        // arguments for example l s_1 .. s_n = l t_1 .. t_n
        // creates (s_1 = t_1, j) ... (s_n = t_n, j).
        else if t.head_is_local() && u.head_is_local() {

        }

        else if t.head_is_global() &&
                u.head_is_global() && t.head == u.head {
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

        else if t.is_forall() && u.is_forall() {
            panic!()
        } else {
            if t.is_stuck() || u.is_stuck() {
                panic!() // t = u
            } else {
                panic!("use judgement to report error")
            }
        }
    }
}
