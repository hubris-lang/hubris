// This style of elaboration is heavily inspired by the elaboration procedure used in
// lean (http://arxiv.org/pdf/1505.04324v2.pdf).


use std::cmp::{PartialOrd, Ordering};
use std::collections::{HashMap, HashSet};
use std::fmt::{self, Debug, Formatter, Display};
use std::rc::Rc;

use core::{Term, Name};
use hubris_syntax::ast::Span;

pub type ConstraintSeq = Vec<Constraint>;

#[derive(Debug, Clone)]
pub enum Constraint {
    Unification(Term, Term, Justification),
    Choice(Term, Term, ChoiceProcedure, Justification),
}

impl PartialEq for Constraint {
    fn eq(&self, other: &Constraint) -> bool {
        use self::Constraint::*;

        match (self, other) {
            (&Unification(ref t1, ref u1, ref j1), &Unification(ref t2, ref u2, ref j2)) => {
                t1 == t2 && u1 == u2 && j1 == j2
            },
            (_, _) => false,
        }
    }
}

impl Eq for Constraint {}

#[derive(Clone)]
struct ChoiceProcedure(Rc<Fn(Term, Term, HashMap<Name, (Term, Justification)>) -> ()>);

impl Debug for ChoiceProcedure {
    fn fmt(&self, fmt: &mut fmt::Formatter) -> Result<(), fmt::Error> {
        panic!()
    }
}

impl Constraint {
    /// Categorizes a constraint into one of constraint categories,
    /// this will also cannonicalize the constraints so that that
    /// the solver does not have to deal with some symmetric cases.
    pub fn categorize(self) -> CategorizedConstraint {
        use self::Constraint::*;
        use self::ConstraintCategory::*;

        match self {
            Unification(t, u, j) => {
                println!("t: {}", t);
                println!("u: {}", u);

                let (t_head, t_args) = t.uncurry();
                let (u_head, u_args) = u.uncurry();

                println!("t_head: {}", t_head);
                println!("u_head: {}", u_head);

                // if t_head.is_qual() && u_head.is_qual() && t_head == u_head {
                //     panic!("delta")
                // } else
                if t_head.is_meta() && u_head.is_meta() {
                    CategorizedConstraint {
                        constraint: Unification(t.clone(), u.clone(), j),
                        category: FlexFlex,
                    }
                } else if t_head.is_meta() || u_head.is_meta() {
                    // This case is either a pattern, quasi-pattern, or flex-rigid
                    // constraint.
                    let (left, right) = if t_head.is_meta() {
                        (t.clone(), u.clone())
                    } else {
                        (u.clone(), t.clone())
                    };

                    let (left_head, left_args) = left.uncurry();

                    for left_arg in &left_args {
                        println!("left_arg: {} {}", left_arg, left_arg.is_constant());
                    }

                    let args_are_constants =
                        left_args.iter().all(|a| a.is_constant());

                    let args_are_distinct =
                        left_args.iter().collect::<HashSet<_>>().len() == left_args.len();

                    if args_are_constants && args_are_distinct {
                        CategorizedConstraint {
                            constraint: Unification(left, right, j),
                            category: Pattern,
                        }
                    } else if args_are_constants {
                        CategorizedConstraint {
                            constraint: Unification(left, right, j),
                            category: QuasiPattern,
                        }
                    } else {
                        CategorizedConstraint {
                            constraint: Unification(left, right, j),
                            category: FlexRigid,
                        }
                    }
                } else {
                    panic!("not sure how to categorize constraint")
                }
            }
            Choice(..)=> panic!(),
        }
    }
}

impl Display for Constraint {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        match self {
            &Constraint::Unification(ref t, ref u, ref j) =>
                write!(formatter, "{} = {} by {:?}", t, u, j),
            _ => panic!()
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Justification {
    Asserted(AssertedBy),
    Assumption,
    Join(Rc<Justification>, Rc<Justification>)
}

// impl Display for Justification {
//     fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
//         use self::Justification::*;
//
//         match self {
//             &Asserted(ref by) => by.fmt(formatter),
//             &Assumption => write!(formatter, "assumption"),
//             &Join(ref j1, ref j2) => {
//                 write!(formatter, "{} <> {}", j1, j2)
//             }
//         }
//     }
// }

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssertedBy {
    Application(Span, Term, Term),
    ExpectedFound(Term, Term),
}

impl Display for AssertedBy {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        use self::AssertedBy::*;

        match self {
            &Application(span, ref u, ref t) =>
                write!(formatter, "applied {} to {}", u, t),
            &ExpectedFound(ref ty, ref infer_ty) =>
                write!(formatter, "expected {} found {}", ty, infer_ty),
        }
    }
}

pub trait Join {
    fn join(self, j: Justification) -> Self;
}

impl Join for Justification {
    fn join(self, j: Justification) -> Self {
        // TODO: Check this out
        Justification::Join(Rc::new(self.clone()), Rc::new(j))
    }
}

#[derive(Debug, Clone, PartialEq, Eq)]
pub struct CategorizedConstraint {
    pub category: ConstraintCategory,
    pub constraint: Constraint,
}

impl Ord for CategorizedConstraint {
    fn cmp(&self, other: &CategorizedConstraint) -> Ordering {
        self.category.partial_cmp(&other.category).unwrap()
    }
}

impl PartialOrd for CategorizedConstraint {
    fn partial_cmp(&self, other: &CategorizedConstraint) -> Option<Ordering> {
        self.category.partial_cmp(&other.category)
    }
}

#[allow(dead_code)]
#[derive(Copy, Clone, Debug, PartialEq, Eq, Hash, Ord)]
pub enum ConstraintCategory {
    Delta,
    Pattern,
    QuasiPattern,
    FlexRigid,
    FlexFlex,
    Recursor,
    Ready,
    Regular,
    Postponed,
    OnDemand,
}

impl PartialOrd for ConstraintCategory {
    fn partial_cmp(&self, other: &ConstraintCategory) -> Option<Ordering> {
        fn to_ordinal(cc: &ConstraintCategory) -> u8 {
            use self::ConstraintCategory::*;

            match *cc {
                FlexFlex => 0,
                Postponed => 1,
                Recursor => 2,
                FlexRigid => 3,
                QuasiPattern => 4,
                Delta => 5,
                Regular => 6,
                Ready => 7,
                Pattern => 8,
                OnDemand => 9,
            }
        }

        to_ordinal(self).partial_cmp(&to_ordinal(other))
    }
}

pub fn constrain<T>(value: T, constraints: ConstraintSeq) -> (T, ConstraintSeq) {
    (value, constraints)
}
