use super::{Error, TyCtxt};

pub fn report_type_error(ty_cx: &TyCtxt, e: Error) {
    match e {
        Error::UnknownVariable(name) => {
            let (offset, line) = ty_cx.source_map.find_line(name.span.lo).unwrap();
            panic!("unknown variable `{}` on: `{}`", name, line);
        },
        Error::UnificationErr(_, t1, t2) => {
            panic!("unification failed `{}` is not equivalent to `{}`", t1, t2);
        }
        err => panic!("error encountered while type checking: {:?}", err),
    }
}
