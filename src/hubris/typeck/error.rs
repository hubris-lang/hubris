use super::super::ast::{Span, HasSpan};
use super::super::core::{Term, Name};
use super::super::error_reporting::{Report, ErrorContext};
use super::TyCtxt;
use parser;

use std::io;
use std::io::prelude::*;
use term::{Terminal, color, Result as TResult};

#[derive(Debug)]
pub enum Error {
    ApplicationMismatch(Span, Term, Term, Term, Term),
    DefUnequal(Span, Term, Term, Vec<(Term, Term)>),
    UnknownVariable(Name),
    ElaborationError,
    MkErr,
    NameExists(Name),
    NoMain,
    Many(Vec<Error>),
    Io(io::Error),
    Parser(parser::Error)
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

impl<O: Write, E: ErrorContext<O>> Report<O, E> for Error {
    fn report(self, cx: &mut E) -> TResult<()> {
        match self {
            Error::UnknownVariable(name) => {
                cx.span_error(name.get_span(),
                    format!("unknown variable `{}`", name))
            }
            Error::DefUnequal(span, t1, t2, disequalities) => {
                let msg = format!("the term `{}` is not equivalent to `{}`", t1, t2);

                try!(cx.span_error(span, msg));

                if disequalities.len() > 1 {
                    try!(writeln!(cx.get_terminal(), "in particular:"));

                    for (t, u) in disequalities {
                        try!(writeln!(cx.get_terminal(), "    {} not equal to {}", t, u));
                    }
                }

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
            e => panic!("error with printing support: {:?}", e),
            // Error::Many(errs) => {
            //     for err in errs {
            //         try!(report_type_error(ty_cx, &mut out, err));
            //     }
            // }
        }
    }
}
