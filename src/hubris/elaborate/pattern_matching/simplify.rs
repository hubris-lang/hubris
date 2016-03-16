use super::super::super::ast::{self};
use super::super::super::core::{self, Term};

use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use itertools::Itertools;
use pretty::*;

pub enum PatternType {
    Cases,
}

pub enum SimpleMatchArm {
    Match(SimpleMatch),
    Term(ast::Term),
}

impl Pretty for SimpleMatchArm {
    fn pretty(&self) -> Doc {
        use self::SimpleMatchArm::*;

        match self {
            &Match(ref m) => m.pretty(),
            &Term(ref t) => t.pretty()
        }
    }
}

impl Display for SimpleMatchArm {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        format(self, formatter)
    }
}


pub enum SimplePattern {
    Constructor(ast::Name, Vec<ast::Name>),
    Name(ast::Name),
}

impl Pretty for SimplePattern {
    fn pretty(&self) -> Doc {
        use self::SimplePattern::*;

        match self {
            &Constructor(ref n, ref ns) => {
                let ns: Vec<_> = ns.iter().map(|x| parens(x.pretty())).collect();
                n.pretty() + seperate(&ns[..], &" ".pretty())
            },
            &Name(ref n) => n.pretty()
        }
    }
}

impl Display for SimplePattern {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        format(self, formatter)
    }
}

pub struct SimpleCase {
    pattern: SimplePattern,
    rhs: SimpleMatchArm,
}

impl Pretty for SimpleCase {
    fn pretty(&self) -> Doc {
        "| ".pretty() + self.pattern.pretty() + " => ".pretty() + self.rhs.pretty()
    }
}

impl Display for SimpleCase {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        format(self, formatter)
    }
}

// A struct representing a simple pattern match, i.e
// one that can not have nested patterns.
pub struct SimpleMatch {
    scrutinee: ast::Term,
    cases: Vec<SimpleCase>,
    pattern_type: PatternType,
}

impl Pretty for SimpleMatch {
    fn pretty(&self) -> Doc {
        let cases : Vec<_> = self.cases.iter().map(|x| x.pretty()).collect();
        "match ".pretty() + self.scrutinee.pretty() + " with {\n".pretty() +
        seperate(&cases[..], &"\n".pretty()) + "end".pretty()
    }
}

impl Display for SimpleMatch {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        format(self, formatter)
    }
}

pub fn simplify_match(scrutinee: ast::Term, cases: Vec<ast::Case>) -> SimpleMatch {
    let mut simple_cases = vec![];

    for (pat_head, cases) in cases.into_iter().group_by(|c| unapply(&c.pattern).0) {
        let case_and_sub_pats : Vec<_> =
            cases.iter()
                 .map(|c| (c, unapply(&c.pattern).1))
                 .collect();

        let mut pattern_sets = vec![];
        if case_and_sub_pats.len() > 0 {
            let pat_number = case_and_sub_pats[0].1.len();
            let mut i = 0;
            while i < pat_number {
                let mut patterns = vec![];
                for &(ref case, ref sub_pats) in &case_and_sub_pats {
                    patterns.push(sub_pats[i].clone());
                    println!("{}", sub_pats[i]);
                }
                pattern_sets.push(patterns);
                i += 1;
            }
        } else {
            panic!()
        }

        for set in &pattern_sets {
            println!("-----");
            for p in set {
                println!("pat: {}", p);
            }
        }

        let names =
            (0..pattern_sets.len())
                .map(|i| ast::Name::from_str(&format!("a{}", i)[..]))
                .collect();

        // let rhs =
        let simple_case = SimpleCase {
            pattern: SimplePattern::Constructor(pat_head, names),
            rhs: SimpleMatchArm::Term(ast::Term::Type),
        };

        println!("simple_case: {}", simple_case);

        simple_cases.push(simple_case);
    }
    panic!()
}

fn unapply(pattern: &ast::Pattern) -> (ast::Name, Vec<ast::Pattern>) {
    match pattern {
        &ast::Pattern::Constructor(ref ctor, ref args) => {
            (ctor.clone(), args.clone())
        }
        &ast::Pattern::Name(ref n) => {
            (n.clone(), vec![])
        }
        _ => panic!()
    }
}
