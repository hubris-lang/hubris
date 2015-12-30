use super::{Error, TyCtxt};
use super::super::ast::Span;

pub fn span_error(ty_cx: &TyCtxt, span: Span, message: String) {
    let (line_no, col_no) = ty_cx.source_map.position(span).unwrap();
    let (line, offset) = ty_cx.source_map.find_line(span.lo).unwrap();
    let format = format!("filename:{}:{}: {}:{} error: {}\n{}",
        line_no, col_no, line_no, col_no, message, line);
    panic!("{}", format);
}

pub fn report_type_error(ty_cx: &TyCtxt, e: Error) {
    match e {
        Error::UnknownVariable(name) => {
            span_error(
                ty_cx,
                name.span,
                format!("unknown variable `{}`", name));
        },
        Error::UnificationErr(span, t1, t2) => {
            span_error(
                ty_cx,
                span,
                format!("unification failed `{}` is not equivalent to `{}`", t1, t2));
        }
        err => panic!("error encountered while type checking: {:?}", err),
    }
}
