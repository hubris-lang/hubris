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

pub enum SimplePattern {
    Dummy,
}

impl Pretty for SimplePattern {
    fn pretty(&self) -> Doc {
        panic!()
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
        panic!()
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
    //let mut simple_cases = vec![];
    for (key, group) in cases.into_iter().group_by(|c| unapply(&c.pattern).0) {
        println!("key {}", key);
        if group.len() == 1 {
            let element = group[0].clone();
            println!("key for 1 {}", element);
        } else {
            let cases = group.iter().map(|c| unapply(&c.pattern));
            for (ctor, pats) in cases {
                println!("{}", ctor);
                for pat in pats {
                    println!("{}", pat);
                }
            }
        }
        // let ector = match try!(self.elab_cx.elaborate_name(key)) {
        //     core::Term::Var { name } => name,
        //     _ => panic!()
        // };
        // let ty = ctor_map.remove(&ector).unwrap();
        // println!("{} : {}", ector, ty);
        // if group.len() > 1 {
        //     for case in group {
        //         println!("case {:?}", case);
        //     }
        //     //let mut names = vec![];
        //     //self.simplify_patterns(ty, ector, &mut names, &group[..], case.rh
        // } else {
        //     ;
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
