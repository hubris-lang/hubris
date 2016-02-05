use super::ast::{Span, SourceMap};

use std::io::prelude::*;
use term::{Terminal, color, Result as TResult};

pub trait ErrorContext<O: Write> {
    // I don't particularly like these names - JR
    fn get_source_map(&self) -> &SourceMap;
    fn get_terminal(&mut self) -> &mut Box<Terminal<Output=O> + Send>;

    /// Reports a message at a given location. Underlines the Span.
    fn span_error(&mut self,
                  span: Span,
                  message: String) -> TResult<()> {
        // TODO: We need to know if we wrap around to more then one line.
        let (line_no, col_no) = self.get_source_map().position(span).unwrap_or((0,0));
        let (line_with_padding, marker) = self.get_source_map().underline_span(span).unwrap_or((format!("??"),format!("??")));

        let filename_str = format!("{}:{}:{}: {}:{} ",
                               self.get_source_map().file_name,
                               line_no,
                               col_no,
                               line_no, // this should be the line where we end, not the same line
                               col_no + (span.hi - span.lo)); // this should be the column we end at

        try!(write!(self.get_terminal(), "{}", filename_str));

        try!(self.get_terminal().fg(color::RED));
        try!(write!(self.get_terminal(), "error: "));
        try!(self.get_terminal().reset());
        try!(writeln!(self.get_terminal(), "{}", message));

        let file_str_simple =
            format!("{}:{}: ",
            self.get_source_map().file_name,
            line_no);

        try!(write!(self.get_terminal(), "{} {}", file_str_simple, line_with_padding));

        let mut marker_padding = "".to_string();

        for _ in 0..file_str_simple.len() {
            marker_padding.push(' ');
        }

        try!(write!(self.get_terminal(), "{}", marker_padding));
        try!(self.get_terminal().fg(color::RED));
        try!(writeln!(self.get_terminal(), "{}", marker));
        try!(self.get_terminal().reset());
        try!(self.get_terminal().flush());

        Ok(())
    }
}

pub trait Report<O: Write, E: ErrorContext<O>> {
    fn report(self, cx: &mut E) -> TResult<()>;
}
