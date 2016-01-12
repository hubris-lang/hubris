use ast::{self, SourceMap, Span, HasSpan};
use core;
use typeck::TyCtxt;

use std::collections::{HashMap, HashSet};
use std::path::Path;
use std::iter;

pub fn elaborate_module<P: AsRef<Path>>(
    path: P,
    module: ast::Module,
    source_map: SourceMap) -> core::Module {
    // Elaboration relies on type checking, so first we setup a typing context to use
    // while elaborating the program. We will run the type checker in inference mode
    // to setup constraints, and then we will solve the constraints, and check the
    // term again.
    let mut ty_cx = TyCtxt::empty();
    ty_cx.source_map = source_map;

    let mut ecx = ElabCx {
        module: module,
        constructors: HashSet::new(),
        ty_cx: ty_cx,
    };

    let name = ecx.elaborate_global_name(ecx.module.name.clone());

    // Might be able to avoid this allocation
    let defs = ecx.module
                  .defs
                  .clone()
                  .into_iter()
                  .filter_map(|def| {
                      match &def {
                          &ast::Definition::Data(ref d) => {
                              for ctor in &d.ctors {
                                  ecx.constructors.insert(ctor.0.clone());
                              }
                          },
                          _ => {}
                      }

                      ecx.elaborate_def(def).map(|edef| {
                          match &edef {
                              &core::Definition::Data(ref d) =>
                                    ecx.ty_cx.declare_datatype(d),
                              &core::Definition::Fn(ref f) =>
                                    ecx.ty_cx.declare_def(f),
                              &core::Definition::Extern(ref e) =>
                                    ecx.ty_cx.declare_extern(e),
                          }
                          edef
                      })
                })
                .collect();

    core::Module {
        file_name: path.as_ref().to_owned(),
        name: name,
        defs: defs,
    }
}

struct ElabCx {
    module: ast::Module,
    /// The set of declared constructor names in scope
    /// we need this to differentiate between a name
    /// binding or null-ary constructor in pattern
    /// matching.
    constructors: HashSet<ast::Name>,
    ty_cx: TyCtxt,
}

impl ElabCx {
    pub fn elaborate_def(&self, def: ast::Definition) -> Option<core::Definition> {
        match def {
            ast::Definition::Data(d) => {
                let edata = core::Definition::Data(self.elaborate_data(d));
                Some(edata)
            }
            ast::Definition::Fn(f) => {
                let efn = core::Definition::Fn(self.elaborate_fn(f));
                debug!("elaborate_def: fn={}", efn);
                panic!();
                Some(efn)
            }
            ast::Definition::Extern(e) => {
                let ext = core::Definition::Extern(self.elaborate_extern(e));
                Some(ext)
            }
            ast::Definition::Comment(_) => None
        }
    }

    fn elaborate_data(&self, data: ast::Data) -> core::Data {
        let mut lcx = LocalElabCx::from_elab_cx(self);
        core::Data {
            span: data.span,
            name: self.elaborate_global_name(data.name),
            ty: lcx.elaborate_term(data.ty),
            ctors: data.ctors
                       .into_iter()
                       .map(|(k, v)| (self.elaborate_global_name(k), lcx.elaborate_term(v)))
                       .collect()
        }
    }

    fn elaborate_fn(&self, fun: ast::Function) -> core::Function {
        let mut lcx = LocalElabCx::from_elab_cx(self);

        lcx.enter_scope(fun.args.clone(), move |lcx, args| {
            let name = self.elaborate_global_name(fun.name);
            let ty = lcx.elaborate_term(fun.ty.clone());
            let body = lcx.elaborate_fn_body(fun.body, fun.ty);

            core::Function {
                name: name,
                args: args,
                ty: ty,
                body: body,
            }
        })
    }

    fn elaborate_extern(&self, ext: ast::Extern) -> core::Extern {
        let ast::Extern { span, name, term } = ext;
        core::Extern {
            span: span,
            name: self.elaborate_global_name(name),
            term: LocalElabCx::from_elab_cx(self)
                             .elaborate_term(term),
        }
    }

    fn elaborate_global_name(&self, n: ast::Name) -> core::Name {
        // core::Name::Qual {
        //     components: vec![n.repr],
        //     span: n.span,
        // }
        panic!("bleh")
    }
}

struct LocalElabCx<'ecx> {
    cx: &'ecx ElabCx,
    // We use this to convert between named arguments,
    // and de bruijn indicies.
    binders: HashMap<ast::Name, usize>,
    binder_level: usize,
    metavar_counter: usize,
    // return_ty: Option<ast::Term>,
}

impl<'ecx> LocalElabCx<'ecx>  {
    fn from_elab_cx(ecx: &'ecx ElabCx) -> LocalElabCx<'ecx> {
        LocalElabCx {
            cx: ecx,
            binders: HashMap::new(),
            binder_level: 1,
            metavar_counter: 0
        }
    }

    // We should enter introducing a name
    fn enter_scope<F, R>(&mut self, binders: Vec<(ast::Name, ast::Term)>, body: F) -> R
    where F : FnOnce(&mut LocalElabCx, Vec<(core::Name, core::Term)>) -> R {
        // Bind the variable to the current level, then
        // Names are the binder level - n + 1?
        // If I'm the first binder, and my level is 2 then
        // 1 + 1 = 2, 1 + 2 = 3
        let mut ebinders = vec![];

        let old_level = self.binder_level;
        let old_binders = self.binders.clone();

        for (name, t) in binders.clone() {
            self.binders.insert(name.clone(), self.binder_level);
            self.binder_level += 1;
            let ename = self.elaborate_name(name);
            let eterm = self.elaborate_term(t);
            ebinders.push((ename, eterm));
        }

        let result = body(self, ebinders);

        self.binder_level = old_level;
        self.binders = old_binders;

        result
    }

    // fn new_metavar(&mut self) -> core::Name {
    //     let result = core::Name::Meta { number: self.metavar_counter };
    //     self.metavar_counter += 1;
    //     result
    // }

    fn elaborate_fn_body(&mut self, term: ast::Term, ret_ty: ast::Term) -> core::Term {
        match term {
            ast::Term::Match { span, scrutinee, cases } => {
                // let escrutinee = Box::new(self.elaborate_term(*scrutinee));
                // let ecases = cases.into_iter().map(|c| self.elaborate_case(c)).collect();
                //
                // core::Term::Match {
                //     span: span,
                //     scrutinee: escrutinee,
                //     cases: ecases,
                //     return_predicate: Box::new(self.new_metavar().to_term()),
                // }
                panic!("can't elaborate match currently")
            }
            other => self.elaborate_term(other)
        }
    }

    // fun (x : Nat) (y : Nat) : Nat => x + y
    // \x : Nat (\y : Nat => y + x)
    // \x : Nat (\1 : Nat => 1 + x)
    // \2 : Nat \1 : Nat => 1 + 2
    fn elaborate_term(&mut self, term: ast::Term) -> core::Term {
        match term {
            ast::Term::Literal { span, lit } => core::Term::Literal {
                span: span,
                lit: self.elaborate_literal(lit),
            },
            ast::Term::Var { name, .. } => core::Term::Var {
                name: self.elaborate_name(name)
            },
            ast::Term::Match { span, scrutinee, cases } => {
                // let escrutinee = Box::new(self.elaborate_term(*scrutinee));
                // let ecases = cases.into_iter().map(|c| self.elaborate_case(c)).collect();
                // // core::Term::Match {
                // //     span: span,
                // //     scrutinee: escrutinee,
                // //     cases: ecases
                // // }
                panic!("match doesn't work right now")
            }
            ast::Term::App { fun, arg, span } => {
                let efun = self.elaborate_term(*fun);
                let earg = self.elaborate_term(*arg);
                core::Term::App {
                    span: span,
                    fun: Box::new(efun),
                    arg: Box::new(earg),
                }
            },
            ast::Term::Forall { name, ty, term, span } =>
                self.enter_scope(vec![(name.clone(), *ty)], move |lcx, binder| {
                    let (name, ty) = binder[0].clone();
                    core::Term::Forall {
                        span: span,
                        name: name,
                        ty: Box::new(ty),
                        term: Box::new(lcx.elaborate_term(*term)),
                    }
                }),
            ast::Term::Metavar { .. } => panic!("can't elaborate meta-variables"),
            ast::Term::Lambda { args, ret_ty, body, span } => {
                self.enter_scope(args, move |lcx, eargs| {
                    let eret_ty = lcx.elaborate_term(*ret_ty);
                    let ebody = lcx.elaborate_term(*body);
                    core::Term::Lambda {
                        span: span,
                        args: eargs,
                        ret_ty: Box::new(eret_ty),
                        body: Box::new(ebody),
                    }
                })
            }
            ast::Term::Type => core::Term::Type,
        }
    }

    fn elaborate_literal(&self, lit: ast::Literal) -> core::Literal {
        match lit {
            ast::Literal::Unit => core::Literal::Unit,
            ast::Literal::Int(i) => core::Literal::Int(i),
        }
    }

    // fn elaborate_case(&mut self, case: ast::Case) -> core::Case {
    //     match case {
    //         ast::Case { pattern, rhs, .. } => {
    //             let names = self.bound_names(&pattern)
    //                             .into_iter()
    //                             .zip(iter::repeat(ast::Term::Type))
    //                             .collect();
    //
    //             self.enter_scope(names, move |lcx, _| {
    //                 core::Case {
    //                     pattern: lcx.elaborate_pattern(pattern),
    //                     rhs: lcx.elaborate_term(rhs)
    //                 }
    //             })
    //         }
    //     }
    // }

    // fn bound_names(&self, pat: &ast::Pattern) -> Vec<ast::Name> {
    //     let mut result = vec![];
    //     match pat {
    //         &ast::Pattern::Name(ref n) => {
    //             if !self.cx.constructors.contains(n) {
    //                 result.push(n.clone())
    //             }
    //         },
    //         &ast::Pattern::Constructor(_, ref pats) => {
    //             for pat in pats {
    //                 if let &ast::Pattern::Name(ref n) = pat {
    //                     result.push(n.clone())
    //                 } else {
    //                     panic!("elaboration should of remove patterns of this form")
    //                 }
    //             }
    //         }
    //         _ => {}
    //     }
    //
    //     debug!("bound_names: result={:?}", result);
    //     result
    // }
    //
    // fn elaborate_pattern(&self, pattern: ast::Pattern) -> core::Pattern {
    //     match pattern {
    //         ast::Pattern::Name(n) =>
    //             core::Pattern::Simple(
    //                 core::SimplePattern::Name(self.elaborate_name(n))),
    //         ast::Pattern::Constructor(n, patterns) =>
    //             core::Pattern::Constructor(
    //                 self.elaborate_name(n),
    //                 patterns.into_iter()
    //                         .map(|p| self.elaborate_simple_pattern(p))
    //                         .collect()),
    //         _ => panic!("elaboration error")
    //     }
    // }
    //
    // fn elaborate_simple_pattern(&self, pattern: ast::Pattern) -> core::SimplePattern {
    //     match pattern {
    //         ast::Pattern::Name(n) =>
    //             core::SimplePattern::Name(self.elaborate_name(n)),
    //         _ => panic!("elaboration error")
    //     }
    // }

    fn elaborate_name(&self, n: ast::Name) -> core::Name {
        debug!("elaborate_name: binders={:?}", self.binders);
        // match self.binders.get(&n) {
        //     None => core::Name::Qual {
        //         span: n.span,
        //         components: vec![n.repr],
        //     },
        //     Some(index) => core::Name::DeBruijn {
        //         span: n.span,
        //         index: *index,
        //         repr: n.repr,
        //     },
        // }
        panic!()
    }
}
