pub type Name = String;

#[derive(Debug)]
pub struct Module {
    pub name: Name,
    pub defs: Vec<Definition>,
}

#[derive(PartialEq, Debug)]
pub struct Schema {
    pub attrs: Vec<(Name, Type)>
}

#[derive(Debug, PartialEq)]
pub enum Definition {
    Schema(Schema),
    Fn(Function)
}

#[derive(Debug, PartialEq)]
pub struct Function {
    pub name: Name,
    pub ty: Type,
    pub body: Term,
}

#[derive(Debug, PartialEq)]
pub enum Term {
    Var(Name),
    Expr(Expr),
    Match(Box<Term>, Vec<Case>),
    App(Box<Term>, Box<Term>),
}

#[derive(Debug, PartialEq)]
pub enum Expr {
    Int(i64) // will need to revisit this decision
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
}

#[derive(Debug, PartialEq)]
pub enum Type {
    Nominal(Name)
}
