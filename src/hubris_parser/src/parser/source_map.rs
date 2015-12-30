use super::super::ast::Span;

pub struct SourceMap {
    pub source: String,
    lines: Vec<(usize, usize)>
}

impl SourceMap {
    pub fn new(source: String) -> SourceMap {
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
}
