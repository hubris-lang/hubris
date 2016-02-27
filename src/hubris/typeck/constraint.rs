// This style of elaboration is heavily inspired by the elaboration procedure used in
// lean (http://arxiv.org/pdf/1505.04324v2.pdf).

use std::cmp::{PartialOrd, Ordering};
use std::fmt::{self, Formatter, Display};
use std::rc::Rc;

use core::Term;

pub type ConstraintSeq = Vec<Constraint>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constraint {
    Unification(Term, Term, Justification),
    Choice(Term),
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
                match (&t, &u) {
                    // We match to "assert" in this branch that terms
                    // should both be applications.
                    (&Term::App { .. }, &Term::App { .. })
                    => {
                        let t_head = t.head().unwrap();
                        let u_head = u.head().unwrap();
                        let t_args = t.args().unwrap();
                        let u_args = u.args().unwrap();
                        println!("t_head: {}", t_head);
                        println!("u_head: {}", u_head);
                        CategorizedConstraint {
                            constraint: Unification(t.clone(), u.clone(), j),
                            category: Pattern,
                        }
                    }
                    // The only other case should be two terms that are just names like,
                    // ?m = Unit or ?m1 = ?m2.
                    (&Term::Var { ref name }, other_side) |
                    (other_side, &Term::Var { ref name })=> {
                        if name.is_meta() && other_side.is_meta() {
                            CategorizedConstraint {
                                constraint: Unification(
                                    Term::Var { name: name.clone() },
                                    other_side.clone(),
                                    j),
                                category: FlexFlex,
                            }
                        } else if name.is_meta() && !other_side.is_meta() {
                            CategorizedConstraint {
                                constraint: Unification(
                                    Term::Var { name: name.clone() },
                                    other_side.clone(),
                                    j),
                                category: Pattern,
                            }
                        } else if !name.is_meta() && other_side.is_meta() {
                            CategorizedConstraint {
                                constraint: Unification(
                                    other_side.clone(),
                                    Term::Var { name: name.clone() },
                                    j),
                                category: Pattern,
                            }
                        } else {
                            panic!("not sure how to categorize this constraint");
                        }
                    }
                    p => panic!("ICE {} = {}, {:?} {:?}", p.0, p.1, p.0.head(), p.1.head())
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

impl Display for Justification {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        use self::Justification::*;

        match self {
            &Asserted(ref by) => by.fmt(formatter),
            &Assumption => write!(formatter, "assumption"),
            &Join(ref r1, ref sr2) => write!(formatter, "assumption"),
        }
    }
}

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum AssertedBy {
    Application(Term, Term),
    ExpectedFound(Term, Term),
}

impl Display for AssertedBy {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        use self::AssertedBy::*;

        match self {
            &Application(ref u, ref t) =>
                write!(formatter, "applied {} to {}", u, t),
            &ExpectedFound(ref infer_ty, ref ty) =>
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

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
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
            }
        }

        to_ordinal(self).partial_cmp(&to_ordinal(other))
    }
}

pub fn constrain<T>(value: T, constraints: ConstraintSeq) -> (T, ConstraintSeq) {
    (value, constraints)
}
