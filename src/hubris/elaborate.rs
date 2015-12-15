use ast;
use core;

pub fn elaborate_module(module: ast::Module) -> core::Module {
    let name = elaborate_name(module.name);
    let defs = module.defs
                     .into_iter()
                     .filter_map(elaborate_def)
                     .collect();

    core::Module {
        name: name,
        defs: defs,
    }
}

pub fn elaborate_name(name: ast::Name) -> core::Name {
    name
}

pub fn elaborate_def(def: ast::Definition) -> Option<core::Definition> {
    match def {
        ast::Definition::Data(d) => Some(core::Definition::Data(elaborate_data(d))),
        ast::Definition::Fn(f) => Some(core::Definition::Fn(elaborate_fn(f))),
        ast::Definition::Extern(e) => Some(core::Definition::Extern(elaborate_extern(e))),
        ast::Definition::Comment(c) => None
    }
}

fn elaborate_data(data: ast::Data) -> core::Data {
    core::Data {
        name: elaborate_name(data.name),
        ctors: data.ctors
                   .into_iter()
                   .map(|(k, v)| (k, elaborate_term(v)))
                   .collect()
    }
}

fn elaborate_fn(fun: ast::Function) -> core::Function {
    core::Function {
        name: elaborate_name(fun.name),
        args: fun.args
                 .into_iter()
                 .map(|(k, v)| (k, elaborate_term(v)))
                 .collect(),
        ty: elaborate_term(fun.ty),
        body: elaborate_term(fun.body),
    }
}

fn elaborate_extern(ext: ast::Extern) -> core::Extern {
    let ast::Extern(n, t) = ext;
    core::Extern(n, elaborate_term(t))
}

fn elaborate_term(term: ast::Term) -> core::Term {
    match term {
        ast::Term::Literal(l) => core::Term::Literal(l),
        ast::Term::Var(n) => core::Term::Var(n),
        ast::Term::Match(scrutinee, cases) => {
            let escrutinee = Box::new(elaborate_term(*scrutinee));
            let ecases = cases.into_iter().map(elaborate_case).collect();
            core::Term::Match(escrutinee, ecases)
        }
        ast::Term::App(f, g) => {
            let ef = elaborate_term(*f);
            let eg = elaborate_term(*g);
            core::Term::App(Box::new(ef), Box::new(eg))
        }
        ast::Term::Forall(x, t, p) =>
            core::Term::Forall(x, Box::new(elaborate_term(*t)),
                            Box::new(elaborate_term(*p))),
        ast::Term::Metavar(n) => panic!("can't elaborate meta-variables"),
        ast::Term::Lambda(vs, rt, body) => panic!(),
        ast::Term::Type => core::Term::Type,
    }
}

fn elaborate_case(term: ast::Case) -> core::Case {
    panic!()
}
