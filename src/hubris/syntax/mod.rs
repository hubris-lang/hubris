use super::parser::{Error};
use session::{Session, Reportable};
use std::io;

impl Reportable for Error {
    fn report(self, session: &Session) -> io::Result<()> {
        match self {
            Error::InvalidToken { location } =>
                session.span_error(location, format!("invalid token")),
            Error::UnrecognizedToken { location, token, expected } =>
                session.span_error(location,
                    format!("unrecognized token {}; expected {:?}", token, expected)),
            Error::UnexpectedEOF { expected } =>
                session.error(format!("unexpected end of file expected {:?}", expected)),
            Error::UserError { error } =>
                session.error(format!("user error: {:?}", error)),
            Error::ExtraTokens { location, token } =>
                session.span_error(location, format!("extra tokens {:?}", token)),
            Error::TokenizerError { location, message } =>
                session.span_error(location, message)
        }
    }
}
