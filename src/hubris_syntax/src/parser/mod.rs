use std::path::Path;
use std::fs::File;
use std::io;
use std::io::Read;

// The parser for our language.
mod hubris;
mod source_map;

use lalrpop_util::ParseError;
pub use self::source_map::SourceMap;
pub use super::tok;

pub struct Parser {
    pub source_map: SourceMap,
}

// TODO: Create a propper error type with the correct Display trait
pub type Error = String;

impl Parser {
    pub fn parse(&self) -> Result<super::ast::Module, Error> {
        let tokenizer = tok::Tokenizer::new(&self.source_map.source[..], 0);
        self.report_error(
            hubris::parse_Module(&self.source_map.source[..],
            tokenizer))
    }

    pub fn parse_term(&self) -> Result<super::ast::Term, Error> {
        let tokenizer = tok::Tokenizer::new(&self.source_map.source[..], 0);
        self.report_error(
            hubris::parse_Term(&self.source_map.source[..],
            tokenizer))
    }

    pub fn report_error<'input, R>(&self,
        result: Result<R, ParseError<usize, tok::Tok<'input>, tok::Error>>) -> Result<R, Error> {
        result.map_err(|e|
            match e {
                ParseError::InvalidToken { location } => {
                    match self.source_map.find_line(location) {
                        Some((offset, line)) => {
                            let mut ptr = "\n".to_string();
                            for _ in 0..offset {
                                ptr.push(' ');
                            }
                            ptr.push('^');
                            format!("invalid_token on:\n {}{}", line, ptr)
                        }
                        None => format!("invalid_token; location unknown")
                    }
                }
                ParseError::UnrecognizedToken { token, expected } => {
                    debug!("{:?} {:?}", token, expected);
                    match token {
                        None => format!("Unexpected EOF"),
                        Some((start, token, end)) => {
                            match self.source_map.find_line(start) {
                                Some((offset, line)) => {
                                    let mut ptr = "\n".to_string();
                                    for _ in 0..offset {
                                        ptr.push(' ');
                                    }
                                    ptr.push('^');
                                    format!("unrecognized_token {:?} on:\n {}{}", token, line, ptr)
                                }
                                None => format!("unrecognized_token {:?}; location unknown", token)
                            }
                        }
                    }
                }
                e => format!("{:?}", e),
            }
        )
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
