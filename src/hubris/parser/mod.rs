use std::path::Path;
use std::fs::File;
use std::io;
use std::io::Read;

// The parser for our language.
mod hubris;

use lalrpop_util::ParseError;

struct SourceMap<'s> {
    source: &'s str,
    lines: Vec<(usize, usize)>
}

impl<'s> SourceMap<'s> {
    fn new(source: &'s str) -> SourceMap<'s> {
        let mut line_start = 0;
        let mut pos = 0;
        let mut lines = Vec::new();

        for c in source.chars() {
            if c == '\n' {
                lines.push((line_start, pos));
                line_start = pos + 1;
            }
            pos += 1;
        }

        SourceMap { source: source, lines: lines }
    }

    fn find_line(&self, index: usize) -> Option<(usize, &str)> {
        for line in &self.lines {
            if index >= line.0 && index <= line.1 {
                return Some((index - line.0, &self.source[line.0..line.1]));
            }
        }

        return None;
    }
}

pub fn from_file<T: AsRef<Path>>(path: T) -> io::Result<Vec<super::ast::Definition>> {
    let path = path.as_ref();

    let mut file = try!(File::open(path));
    let mut s = String::new();

    try!(file.read_to_string(&mut s));

    let source_map = SourceMap::new(&s[..]);

    match hubris::parse_Module(&s[..]) {
        Err(e) => match e {
            ParseError::InvalidToken { location } => {
                let (offset, line) = source_map.find_line(location).unwrap();
                let mut ptr = "\n".to_string();
                for _ in 0..offset {
                    ptr.push(' ');
                }
                ptr.push('^');
                panic!("invalid_token on:\n {}{}", line, ptr);
            }
            ParseError::UnrecognizedToken { token, .. } => {
                let (start, token, end) = token.unwrap();
                let (offset, line) = source_map.find_line(start).unwrap();
                let mut ptr = "\n".to_string();
                for _ in 0..offset {
                    ptr.push(' ');
                }
                ptr.push('^');
                panic!("unrecongnized_token {:?} on:\n {}{}", token, line, ptr);
            }
            e => panic!("{:?}", e),
        },
        Ok(v) => Ok(v)
    }
}
