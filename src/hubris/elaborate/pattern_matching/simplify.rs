use super::super::super::ast::{self};
use super::renamer::{rename_term, RenameMap};

use std::collections::HashMap;
use std::fmt::{self, Display, Formatter};

use pretty::*;

#[derive(Debug, Copy, Clone)]
pub enum PatternType {
    Cases,
}

#[derive(Debug, Clone)]
pub enum SimpleMatchArm {
    Match(SimpleMatch),
    Term(ast::Term),
}

impl SimpleMatchArm {
    fn rename(self, rename_map: &RenameMap) -> SimpleMatchArm {
        use self::SimpleMatchArm::*;

        match self {
            Match(m) => {
                Match(m.rename(rename_map))
            }
            Term(mut t) => {
                rename_term(rename_map.clone(), &mut t);
                Term(t)
            }
        }
    }
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

/// A struct representing a simple pattern match, i.e one that can not have nested patterns.
#[derive(Debug, Clone)]
pub struct SimpleMatch {
    pub scrutinee: ast::Term,
    pub cases: Vec<SimpleCase>,
    pub pattern_type: PatternType,
}

impl SimpleMatch {
    fn rename(self, rename_map: &RenameMap) -> SimpleMatch {
        let mut scrutinee = self.scrutinee;
        rename_term(rename_map.clone(), &mut scrutinee);
        let cases = self.cases.into_iter().map(|c| c.rename(rename_map)).collect();

        SimpleMatch {
            scrutinee: scrutinee,
            cases: cases,
            pattern_type: self.pattern_type,
        }
    }
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

#[derive(Debug, Clone)]
pub struct SimpleCase {
    pub pattern: SimplePattern,
    pub rhs: SimpleMatchArm,
}

impl SimpleCase {
    fn rename(self, rename_map: &RenameMap) -> SimpleCase {
        panic!()
    }
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

#[derive(Debug, Clone)]
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

pub fn simplify_match(scrutinee: ast::Term, cases: Vec<ast::Case>) -> SimpleMatch {
    let mut simple_cases = vec![];

    for case in cases {
        let rhs = SimpleMatchArm::Term(case.rhs);
        simple_cases.push(simplify_pattern(case.pattern, rhs));
    }

    let simple_match = SimpleMatch {
        scrutinee: scrutinee,
        cases: simple_cases,
        pattern_type: PatternType::Cases,
    };

    let simple_match = match condense(SimpleMatchArm::Match(simple_match)) {
        SimpleMatchArm::Match(m) => m,
        _ => panic!("condensing a match should result in at least one match")
    };

    simple_match
}

pub fn condense(simple_match: SimpleMatchArm) -> SimpleMatchArm {
    match simple_match {
        SimpleMatchArm::Term(rhs) => SimpleMatchArm::Term(rhs),
        SimpleMatchArm::Match(simple_match) => {
            let SimpleMatch {
                scrutinee,
                cases,
                pattern_type,
            } = simple_match;

            // This handles the case in which the simplification pass has generated
            // a simple match like `match a with | b => rhs`, we just simplify to
            // rhs.
            if cases.len() == 1 {
                let case = cases[0].clone();
                match case.pattern {
                    SimplePattern::Name(n) => {
                        let mut name_map = HashMap::new();
                        name_map.insert(n, scrutinee.clone());
                        condense(case.rhs).rename(&name_map)
                    }
                    _ => {
                        let cases =
                            cases.into_iter()
                                 .map(|mut case| {
                                     case.rhs = condense(case.rhs);
                                     case
                                 })
                                 .collect();

                        SimpleMatchArm::Match(SimpleMatch {
                            scrutinee: scrutinee,
                            cases: cases,
                            pattern_type: pattern_type,
                        })
                    }
                }
            } else {
                let new_cases = vec![];
                SimpleMatchArm::Match(SimpleMatch {
                    scrutinee: scrutinee,
                    cases: new_cases,
                    pattern_type: PatternType::Cases,
                })
            }
        }
    }
}

pub fn simplify_pattern(pattern: ast::Pattern, mut rhs: SimpleMatchArm) -> SimpleCase {
    match pattern {
        ast::Pattern::Placeholder => {
            panic!()
        }
        ast::Pattern::Name(n) => {
            SimpleCase {
                pattern: SimplePattern::Name(n),
                rhs: rhs,
            }
        }
        ast::Pattern::Constructor(pat_head, pat_args) => {
            println!("pattern_head: {}", pat_head);

            let mut arg_names = vec![];

            for (i, pat_arg) in pat_args.into_iter().enumerate().rev() {
                println!("pattern_arg {}", pat_arg);
                let arg_name = ast::Name::from_str(&format!("a{}", i)[..]);
                arg_names.push(arg_name.clone());
                rhs = SimpleMatchArm::Match(SimpleMatch {
                    scrutinee: ast::Term::Var { name: arg_name, implicit: false },
                    cases: vec![simplify_pattern(pat_arg, rhs)],
                    pattern_type: PatternType::Cases,
                })
            }

            SimpleCase {
                pattern: SimplePattern::Constructor(pat_head, arg_names.into_iter().rev().collect()),
                rhs: rhs,
            }
        }
    }
}
