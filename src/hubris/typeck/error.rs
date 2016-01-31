use super::super::ast::{SourceMap, Span, HasSpan};
use super::super::core::{Term, Name};
use super::super::error_reporting::{Report, span_error};
use super::TyCtxt;

use std::io;
use std::io::prelude::*;
use term::{Terminal, color, Result as TResult};

#[derive(Debug)]
pub enum Error {
    ApplicationMismatch(Span, Term, Term),
    DefUnequal(Span, Term, Term, Vec<(Term, Term)>),
    UnknownVariable(Name),
    ElaborationError,
    MkErr,
    NameExists(Name),
    NoMain,
    Many(Vec<Error>),
    Io(io::Error),
}

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl Report for Error {
    type Context = TyCtxt;

    fn report<O: Write>(ty_cx: &Self::Context,
                        mut out: Box<Terminal<Output = O>>,
                        error: Self)
                        -> TResult<()> {
        match error {
            Error::UnknownVariable(name) => {
                span_error(ty_cx,
                           &mut out,
                           name.get_span(),
                           format!("unknown variable `{}`", name))
            }
            Error::DefUnequal(span, t1, t2, disequalities) => {
                let msg = format!("the term `{}` is not equivalent to `{}`", t1, t2);

                try!(span_error(ty_cx, &mut out, span, msg));

                if disequalities.len() > 1 {
                    try!(writeln!(out, "in particular:"));

                    for (t, u) in disequalities {
                        try!(writeln!(out, "    {} not equal to {}", t, u));
                    }
                }

                Ok(())
            }
            Error::ApplicationMismatch(span, _, _) => {
                let msg = format!("can not apply a term t with type t' to a term u to u'");

                try!(span_error(ty_cx, &mut out, span, msg));

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
