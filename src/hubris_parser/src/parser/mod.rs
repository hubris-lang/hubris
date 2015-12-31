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
    // TODO: Clean up token error handling
    pub fn parse(&self) -> super::ast::Module {
        match hubris::parse_Module(&self.source_map.source[..]) {
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
                ParseError::UnrecognizedToken { token, .. } => {
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
        source_map: SourceMap::new(path.to_owned(), contents),
    })
}
