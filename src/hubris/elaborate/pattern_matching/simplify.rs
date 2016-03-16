use hubris::ast::{self, Span};

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
    pub pattern: SimplePattern,
    pub rhs: SimpleMatchArm,
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
    pub scrutinee: ast::Term,
    pub cases: Vec<SimpleCase>,
    pub pattern_type: PatternType,
}

impl Pretty for SimpleMatch {
    fn pretty(&self) -> Doc {
        let cases : Vec<_> = self.cases.iter().map(|x| x.pretty()).collect();
        "match ".pretty() + self.scrutinee.pretty() + " with\n".pretty() +
        seperate(&cases[..], &"\n".pretty()) + "\nend".pretty()
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


        let mut names = vec![];
        let mut pattern_sets = vec![];

        if case_and_sub_pats.len() > 0 {
            let pat_number = case_and_sub_pats[0].1.len();

            let mut i = 0;
            let mut j = 0;

            while i < pat_number {
                let mut patterns = vec![];
                for &(ref case, ref sub_pats) in &case_and_sub_pats {
                    patterns.push(sub_pats[i].clone());
                    println!("{}", sub_pats[i]);
                    j += 1;
                }
                names.push(ast::Name::from_str(&format!("a{}", j)));
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

        let rhses : Vec<_> = cases.iter().map(|x| &x.rhs).collect();
        let rhs = construct_pattern_match(&names[..], &pattern_sets[..], &rhses[..]);

        let simple_case = SimpleCase {
            pattern: SimplePattern::Constructor(pat_head, names),
            rhs: rhs,
        };

        println!("simple_case: {}", simple_case);

        simple_cases.push(simple_case);
    }

    SimpleMatch {
        scrutinee: scrutinee,
        cases: simple_cases,
        pattern_type: PatternType::Cases,
    }
}

fn construct_pattern_match(names: &[ast::Name], pattern_sets: &[Vec<ast::Pattern>], rhss: &[&ast::Term]) -> SimpleMatchArm {
    assert_eq!(names.len(), pattern_sets.len());
    if names.len() == 0 && pattern_sets.len() == 0 {
        SimpleMatchArm::Term(rhss[0].clone())
    } else {
        let cases : Vec<_> =
            pattern_sets[0]
                .iter()
                .enumerate()
                .map(|(i, x)|
                    ast::Case {
                        pattern: x.clone(),
                        rhs: rhss[i].clone(),
                        span: Span::dummy(),
                }).collect();

        let mat = ast::Term::Match {
            scrutinee: Box::new(ast::Term::Var { name: names[0].clone() }),
            cases: cases.clone(),
            span: Span::dummy(),
        };

        let simple_match = simplify_match(
            ast::Term::Var { name: names[0].clone() },
            cases);

        SimpleMatchArm::Match(simple_match)
    }
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
