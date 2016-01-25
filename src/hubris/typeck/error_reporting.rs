use super::{Error, TyCtxt};
use super::super::ast::{Span, HasSpan};

use std::io;
use std::io::prelude::*;
use term::{Terminal, color, Result as TResult};

pub fn span_error<O: Write>(ty_cx: &TyCtxt,
                            mut term: Box<Terminal<Output=O>>,
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
    out: Box<Terminal<Output=O>>,
    e: Error) -> TResult<()>
{
    match e {
        Error::UnknownVariable(name) => {
            span_error(
                ty_cx,
                out,
                name.get_span(),
                format!("unknown variable `{}`", name))
        },
        Error::UnificationErr(span, t1, t2, disequalities) => {
            let mut msg = format!(
                "the term `{}` is not equivalent to `{}`",
                t1,
                t2);

            msg.push_str("\n\nparticularly:");

            for (t, u) in disequalities {
                msg.push_str(&format!("    {} not equal to {}", t, u));
            }

            span_error(
                ty_cx,
                out,
                span,
                msg)
        }
        Error::ApplicationMismatch(span, t, u) => {
            panic!("{} {}", t, u);
        }
        _ => panic!("unhandled error")
    }
}
