// This style of elaboration is heavily inspired by the elaboration procedure used in
// lean (http://arxiv.org/pdf/1505.04324v2.pdf).

use core::Term;

pub type ConstraintSeq = Vec<Constraint>;

enum Constraint {
    Unification(Term, Term, Justification),
    Choice,
}

enum Justification {
    Asserted,
    Assumption,
    Join,
}

// A trait representing any type that can be constrained, i.e contains meta-variables.
// trait Constrain {
// }

pub fn constrain<T>(value: T, constraints: ConstraintSeq) -> (T, ConstraintSeq) {
    (value, constraints)
}
