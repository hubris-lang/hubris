use super::parser;
use super::super::error_reporting::{Report, ErrorContext};

impl ErrorContext for Parser {
    fn get_source_map(&self) -> &SourceMap {
        &self.source_map
    }

    fn get_terminal(&mut self) -> &mut Box<Terminal<Output=io::Stdout> + Send> {
        &mut self.terminal
    }
}

impl<O: Write, E: ErrorContext<O>> Report<O, E> for Error {
    fn report(self, cx: &mut E) -> TResult<()> {
        match self {
            Error::InvalidToken { location } =>
                match self.source_map.find_line(location) {
                    Some((offset, line)) => {
                        let mut ptr = "\n".to_string();
                        for _ in 0..offset {
                            ptr.push(' ');
                        }
                        ptr.push('^');
                        writeln!(cx.get_terminal(),
                            format!("invalid_token on:\n {}{}", line, ptr));
                    }
                    None => writeln!(cx.get_terminal,
                                format!("invalid_token; location unknown"));
                },
            Error::UnrecognizedToken { (start, end), token, expected } =>
                match self.source_map.find_line(location) {
                    Some((offset, line)) => {
                        let mut ptr = "\n".to_string();
                        for _ in 0..offset {
                            ptr.push(' ');
                        }
                        ptr.push('^');
                        writeln!(cx.get_terminal(), format!("unrecognized token {} on:\n {}{}\nexpected {:?}", token, line, ptr, expected));
                    }
                    None => writeln!(cx.get_terminal, format!("unrecognized token {}; location unknown\nexpected {:?}", token, expected)),
                },
            Error::UnexpectedEOF { expected } => writeln!(cx.get_terminal, format!("unexpected EOF; expected {:?}", expected)),
            Error::UserError { error } => writeln!(cx.get_terminal, format!("{:?}", error))
        }
    }
}
