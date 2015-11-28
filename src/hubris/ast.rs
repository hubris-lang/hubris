use std::path::PathBuf;

pub type Name = String;

#[derive(Debug)]
pub struct Module {
    pub name: Name,
    pub defs: Vec<Definition>,
}

impl Module {
    pub fn file_name(&self) -> PathBuf {
        PathBuf::from(&self.name[..])
    }
}

#[derive(PartialEq, Debug)]
pub struct Schema {
    pub attrs: Vec<(Name, Term)>
}

#[derive(PartialEq, Debug)]
pub struct Data {
    pub attrs: Vec<(Name, Term)>
}

#[derive(Debug, PartialEq)]
pub struct Extern(pub Name, pub Term);

#[derive(Debug, PartialEq)]
pub enum Definition {
    Schema(Schema),
    Data(Schema),
    Fn(Function),
    Extern(Extern),
    Comment(())
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Name,
    pub args: Vec<(Name, Term)>,
    pub ty: Term,
    pub body: Term,
}

#[derive(Debug, PartialEq)]
pub enum Term {
    Literal(Literal),
    Var(Name),
    Match(Box<Term>, Vec<Case>),
    App(Box<Term>, Box<Term>),
    Forall(Name, Box<Term>, Box<Term>),
    Metavar(Name),
    Lambda(Vec<(Name, Term)>, Box<Term>, Box<Term>),
    Type,
}

#[derive(Debug, PartialEq)]
pub enum Literal {
    Int(i64), // will need to revisit this decision
    Unit
}

#[derive(Debug, PartialEq)]
pub struct Case {
    pub pattern: Pattern,
    pub rhs: Term,
}

#[derive(Debug, PartialEq)]
pub enum Pattern {
    Name(Name),
    Constructor(Name, Vec<Pattern>),
    Placeholder,
}
