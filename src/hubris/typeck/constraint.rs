// This style of elaboration is heavily inspired by the elaboration procedure used in
// lean (http://arxiv.org/pdf/1505.04324v2.pdf).

use std::fmt::{self, Formatter, Display};
use std::cmp::{PartialOrd, Ordering};

use core::Term;

pub type ConstraintSeq = Vec<Constraint>;

#[derive(Debug, Clone, PartialEq, Eq, Hash)]
pub enum Constraint {
    Unification(Term, Term, Justification),
    Choice,
}

impl Constraint {
    pub fn categorize(self) -> CategorizedConstraint {
        use self::Constraint::*;
        use self::ConstraintCategory::*;

        let category = match &self {
            &Unification(ref t, ref u, ref j) => {
                println!("t: {}", t);
                println!("u: {}", u);
                Pattern
            }
            &Choice => panic!(),
        };

        CategorizedConstraint {
            constraint: self,
            category: category
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
    Asserted,
    Assumption,
    Join(Box<Justification>, Box<Justification>)
}

pub trait Join {
    fn join(self, j: Justification) -> Self;
}

impl Join for Justification {
    fn join(self, j: Justification) -> Self {
        panic!()
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
