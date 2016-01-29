use std::path::Path;
use std::fs::File;
use std::io;
use std::io::Read;

// The parser for our language.
mod hubris;
mod source_map;

use lalrpop_util::ParseError;
pub use self::source_map::SourceMap;

pub struct Parser {
    pub source_map: SourceMap,
}

impl Parser {
    pub fn parse(&self) -> super::ast::Module {
        self.report_error(hubris::parse_Module(&self.source_map.source[..]))
    }

    pub fn parse_term(&self) -> super::ast::Term {
        self.report_error(hubris::parse_Term(&self.source_map.source[..]))
    }

    pub fn report_error<'input, R>(&self,
        result: Result<R, ParseError<usize,(usize, &'input str),()>>) -> R {
        match result {
            Err(e) => match e {
                ParseError::InvalidToken { location } => {
                    let (offset, line) = self.source_map.find_line(location).unwrap();
                    let mut ptr = "\n".to_string();
                    for _ in 0..offset {
                        ptr.push(' ');
                    }
                    ptr.push('^');
                    panic!("invalid_token on:\n {}{}", line, ptr);
                }
                ParseError::UnrecognizedToken { token, expected } => {
                    debug!("{:?} {:?}", token, expected);
                    let (start, token, end) = token.unwrap();
                    let (offset, line) = self.source_map.find_line(start).unwrap();
                    let mut ptr = "\n".to_string();
                    for _ in 0..offset {
                        ptr.push(' ');
                    }
                    ptr.push('^');
                    panic!("unrecongnized_token {:?} on:\n {}{}", token, line, ptr);
                }
                e => panic!("{:?}", e),
            },
            Ok(v) => v
        }
    }
}

pub fn from_file<T: AsRef<Path>>(path: T) -> io::Result<Parser> {
    let path = path.as_ref();

    let mut file = try!(File::open(path));
    let mut contents = String::new();

    try!(file.read_to_string(&mut contents));

    Ok(Parser {
        source_map: SourceMap::from_file(
            format!("{}", path.to_owned().display()),
            contents),
    })
}

pub fn from_string(contents: String) -> io::Result<Parser> {
    let source_map = SourceMap::from_source(contents);

    Ok(Parser {
        source_map: source_map,
    })
}
