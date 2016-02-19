use super::parser::{Error};
use session::{Session, Reportable};
use term::{Result as TResult, Error as TError};
use std::io::{self, Write};

impl Reportable for Error {
    fn report(self, session: &Session) -> io::Result<()> {
        match self {
            Error::InvalidToken { location } =>
                session.span_error(location, format!("invalid token")),
            Error::UnrecognizedToken { location, token, expected } =>
                session.span_error(location,
                    format!("unrecognized token {}; expected {:?}", token, expected)),
            Error::UnexpectedEOF { expected } =>
                // session.error("unexpected EOF; expected {:?}", expected),
                panic!(),
            Error::UserError { error } =>
                // writeln!(session.get_terminal(), "user error: {:?}", error).map_err(TError::Io),
                panic!(),
            Error::ExtraTokens { location, token } =>
                session.span_error(location, format!("extra tokens {:?}", token)),
            Error::TokenizerError { location, message } =>
                session.span_error(location, message)
        }
    }
}
