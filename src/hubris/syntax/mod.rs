use super::parser::{Error};
use super::error_reporting::{Report, ErrorContext};
use term::{Result as TResult, Error as TError};
use std::io::{Write};

impl<O: Write, E: ErrorContext<O>> Report<O, E> for Error {
    fn report(self, cx: &mut E) -> TResult<()> {
        match self {
            Error::InvalidToken { location } =>
                cx.span_error(location, format!("invalid token")),
            Error::UnrecognizedToken { location, token, expected } =>
                cx.span_error(location,
                    format!("unrecognized token {}; expected {:?}", token, expected)),
            Error::UnexpectedEOF { expected } =>
                writeln!(cx.get_terminal(), "unexpected EOF; expected {:?}", expected)
                    .map_err(TError::Io),
            Error::UserError { error } =>
                writeln!(cx.get_terminal(), "user error: {:?}", error).map_err(TError::Io),
            Error::ExtraTokens { location, token } =>
                cx.span_error(location, format!("extra tokens {:?}", token)),
            Error::TokenizerError { location, message } =>
                cx.span_error(location, message)
        }
    }
}
