use typeck::{TyCtxt};
use super::ast::{Span};

use std::io::prelude::*;
use term::{Terminal, color, Result as TResult};

pub trait Report {
    type Context;

    fn report<O: Write>(cx: &Self::Context,
                         mut out: Box<Terminal<Output = O>>,
                         error: Self)
                         -> TResult<()>;
}

pub fn span_error<O: Write>(ty_cx: &TyCtxt,
                            term: &mut Box<Terminal<Output = O>>,
                            span: Span,
                            message: String)
                            -> TResult<()> {
    let (line_no, col_no) = ty_cx.source_map.position(span).unwrap();
    let (line_with_padding, marker) = ty_cx.source_map.underline_span(span).unwrap();

    let filename_str = format!("{}:{}:{}: {}:{} ",
                               ty_cx.source_map.file_name,
                               line_no,
                               col_no,
                               line_no,
                               col_no);

    try!(write!(term, "{}", filename_str));

    try!(term.fg(color::RED));
    try!(write!(term, "error: "));
    try!(term.reset());
    try!(writeln!(term, "{}", message));

    let file_str_simple = format!("{}:{}:{}: ", ty_cx.source_map.file_name, line_no, col_no);

    try!(write!(term, "{} {}", file_str_simple, line_with_padding));

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
