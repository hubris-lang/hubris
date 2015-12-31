use super::super::ast::Span;
use std::path::PathBuf;

pub struct SourceMap {
    pub file_name: PathBuf,
    pub source: String,
    lines: Vec<(usize, usize)>
}

impl SourceMap {
    pub fn new(file_name: PathBuf, source: String) -> SourceMap {
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

        SourceMap {
            file_name: file_name,
            source: source,
            lines: lines
        }
    }

    // Given a span returns the line and column number information.
    pub fn position(&self, span: Span) -> Option<(usize, usize)> {
        for (i, line) in self.lines.iter().enumerate() {
            if span.lo >= line.0 && span.lo <= line.1 {
                return Some((i, span.lo - line.0))
            }
        }

        return None;
    }

    pub fn find_line(&self, index: usize) -> Option<(usize, &str)> {
        for line in &self.lines {
            if index >= line.0 && index <= line.1 {
                return Some((index - line.0, &self.source[line.0..line.1]));
            }
        }

        return None;
    }

    pub fn underline_span(&self, span: Span) -> Option<(String, String)> {
        self.find_line(span.lo).map(|(offset, line)| {
            let mut line_with_padding = line.to_owned();
            line_with_padding.push('\n');

            // Add the correct # of spaces on the marker line
            for _ in 0..offset {
                line_with_padding.push(' ');
            }

            // Mark the start of the span.
            let mut marker = "^".to_string();
            for _ in 0..(span.hi - span.lo) {
                marker.push('~');
            }

            (line_with_padding, marker)
        })
    }
}
