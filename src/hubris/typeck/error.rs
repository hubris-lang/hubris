use super::super::ast::{Span, HasSpan};
use super::super::core::{Term, Name};
use super::super::session::{Reportable, Session};
use parser;
use super::solver;

use std::io;
use term;

#[derive(Debug)]
pub enum Error {
    ExpectedFunction(Span, Term),
    ApplicationMismatch(Span, Term, Term, Term, Term),
    DefUnequal(Span, Term, Term, Vec<(Term, Term)>),
    UnknownVariable(Name),
    ElaborationError,
    MkErr,
    NameExists(Name),
    NoMain,
    Many(Vec<Error>),
    Io(io::Error),
    Parser(parser::Error),
    Term(term::Error),
    Solver(solver::Error),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Error {
        Error::Parser(err)
    }
}

impl From<term::Error> for Error {
    fn from(err: term::Error) -> Error {
        Error::Term(err)
    }
}

impl From<solver::Error> for Error {
    fn from(err: solver::Error) -> Error {
        Error::Solver(err)
    }
}

impl Reportable for Error {
    fn report(self, cx: &Session) -> io::Result<()> {
        match self {
            Error::UnknownVariable(name) => {
                cx.span_error(name.get_span(),
                    format!("unknown variable `{}`", name))
            }
            Error::DefUnequal(span, t1, t2, disequalities) => {
                let msg = format!("the term `{}` is not equivalent to `{}`", t1, t2);

                try!(cx.span_error(span, msg));

                // if disequalities.len() > 1 {
                //     try!(writeln!(cx.get_terminal(), "in particular:"));
                //
                //     for (t, u) in disequalities {
                //         try!(writeln!(cx.get_terminal(), "    {} not equal to {}", t, u));
                //     }
                // }

                Ok(())
            }
            Error::ApplicationMismatch(span, t, u, ty_of_t, ty_of_u) => {
                let msg = format!(
                    "can not apply `{}` with type `{}`\n \
                     to `{}`` with type `{}`",
                    t, u, ty_of_t, ty_of_u);

                try!(cx.span_error(span, msg));

                Ok(())
            }
            Error::ExpectedFunction(span, f) => {
                let msg = format!(
                    "can not apply term with type `{}` to arguments,
                     only terms with function types can be applied", f);

                cx.span_error(span, msg)
            }
            e => panic!("error with printing support: {:?}", e),
            // Error::Many(errs) => {
            //     for err in errs {
            //         try!(report_type_error(ty_cx, &mut out, err));
            //     }
            // }
        }
    }
}
