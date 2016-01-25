use hubris_parser::ast::{Span, HasSpan};

use std::fmt::{self, Display, Formatter};
use std::hash::{Hash, Hasher};

use super::Term;

#[derive(Clone, Debug, Eq)]
pub enum Name {
    DeBruijn { index: usize, span: Span, repr: String },
    Local { number: usize, repr: String, ty: Box<Term> },
    Qual { span: Span, components: Vec<String> },
    Meta { number: usize, ty: Box<Term> }
}

impl Name {
    pub fn from_str(s: &str) -> Name {
        Name::Qual {
            span: Span::dummy(),
            components: vec![s.to_owned()],
        }
    }

    pub fn to_term(&self) -> Term {
        Term::Var { name: self.clone() }
    }

    pub fn is_placeholder(&self) -> bool {
        use self::Name::*;

        match self {
            &DeBruijn { ref repr, .. } |
            &Local { ref repr, .. } if repr == "" || repr == "_" => true,
            _ => false
        }
    }
}

impl PartialEq for Name {
    fn eq(&self, other: &Name) -> bool {
        use self::Name::*;

        match (self, other) {
            (&DeBruijn { index: ref index1, .. },
             &DeBruijn { index: ref index2, .. }) =>
                index1 == index2,
            (&Qual { components: ref components1, .. },
             &Qual { components: ref components2, .. }) =>
                components1 == components2,
            _ => false,
        }
    }
}

impl Hash for Name {
    fn hash<H>(&self, state: &mut H) where H: Hasher {
        use self::Name::*;

        match self {
            &DeBruijn { ref index, .. } => {
                0.hash(state);
                index.hash(state);
            },
            &Qual { ref components, .. } => {
                1.hash(state);
                components.hash(state);
            }
            &Meta { ref number, .. } => {
                2.hash(state);
                number.hash(state);
            }
            &Local { ref number, .. } => {
                3.hash(state);
                number.hash(state);
            }
        }
    }
}

impl Display for Name {
    fn fmt(&self, formatter: &mut Formatter) -> Result<(), fmt::Error> {
        use self::Name::*;

        match self {
            &DeBruijn { ref repr, .. } =>
                try!(write!(formatter, "{}", repr)),
            &Qual { ref components, .. } =>
                if components.len() == 1 {
                    try!(write!(formatter, "{}", components[0]))
                } else {
                    for c in components {
                        try!(write!(formatter, "{}.", c))
                    }
                },
            &Meta { number, .. } =>
                try!(write!(formatter, "?{}", number)),
            &Local { ref repr, number, .. } =>
                try!(write!(formatter, "{}(local {})", repr, number)),
        }

        Ok(())
    }
}

impl HasSpan for Name {
    fn get_span(&self) -> Span {
        use self::Name::*;

        match self {
            &Qual { span, .. } => span,
            &DeBruijn { span, .. } => span,
            &Meta { .. } => Span::dummy(),
            &Local { .. } => Span::dummy(),
        }
    }

    fn set_span(&mut self, sp: Span) {
        use self::Name::*;

        match self {
            &mut DeBruijn { ref mut span, .. } =>
                *span = sp,
            &mut Qual { ref mut span, ..} =>
                *span = sp,
            &mut Meta { .. } => {},
            &mut Local { .. } => {},
        }
    }
}
