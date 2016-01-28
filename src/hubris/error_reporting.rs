use typeck::{Error as TypeCkError, TyCtxt};
use super::ast::{Span, HasSpan};

use std::io::prelude::*;
use term::{Terminal, color, Result as TResult};

pub fn span_error<O: Write>(ty_cx: &TyCtxt,
                            term: &mut Box<Terminal<Output=O>>,
                            span: Span,
                            message: String) -> TResult<()> {
    let (line_no, col_no) = ty_cx.source_map.position(span).unwrap();
    let (line_with_padding, marker) =
        ty_cx.source_map.underline_span(span).unwrap();

    let filename_str = format!(
        "{}:{}:{}: {}:{} ",
        ty_cx.source_map.file_name.display(),
        line_no,
        col_no,
        line_no,
        col_no);

    try!(write!(
        term,
        "{}",
        filename_str));

    try!(term.fg(color::RED));
    try!(write!(term, "error: "));
    try!(term.reset());
    try!(writeln!(term, "{}", message));

    let file_str_simple =
        format!("{}:{}:{}: ",
            ty_cx.source_map.file_name.display(),
            line_no, col_no);

    try!(write!(
        term,
        "{} {}",
        file_str_simple,
        line_with_padding));

    let mut marker_padding = "".to_string();

    for _ in 0..file_str_simple.len() {
        marker_padding.push(' ');
    }

    try!(write!(term, "{}", marker_padding));
    try!(term.fg(color::RED));
    try!(writeln!(term, "{}", marker));
    try!(term.reset());

    Ok(())
}

pub fn report_type_error<O: Write>(
    ty_cx: &TyCtxt,
    mut out: Box<Terminal<Output=O>>,
    e: TypeCkError) -> TResult<()>
{
    match e {
        TypeCkError::UnknownVariable(name) => {
            span_error(
                ty_cx,
                &mut out,
                name.get_span(),
                format!("unknown variable `{}`", name))
        },
        TypeCkError::UnificationErr(span, t1, t2, disequalities) => {
            let msg = format!(
                "the term `{}` is not equivalent to `{}`",
                t1,
                t2);

            try!(span_error(
                ty_cx,
                &mut out,
                span,
                msg));

            if disequalities.len() > 1 {
                try!(writeln!(out, "in particular:"));

                for (t, u) in disequalities {
                    try!(writeln!(out, "    {} not equal to {}", t, u));
                }
            }

            Ok(())
        }
        TypeCkError::ApplicationMismatch(span, _, _) => {
            let msg = format!(
                "can not apply a term t with type t' to a term u to u'");

            try!(span_error(
                ty_cx,
                &mut out,
                span,
                msg));

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
