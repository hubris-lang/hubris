//! A tokenizer for use in LALRPOP itself.

use std::str::CharIndices;
use unicode_xid::UnicodeXID;

use self::ErrorCode::*;
use self::Tok::*;

#[derive(Clone, Debug, PartialEq, Eq)]
pub struct Error {
    pub location: usize,
    pub code: ErrorCode
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum ErrorCode {
    UnrecognizedToken,
    UnterminatedEscape,
    UnterminatedStringLiteral,
    UnterminatedCode,
    ExpectedStringLiteral,
}

fn error<T>(c: ErrorCode, l: usize) -> Result<T,Error> {
    Err(Error { location: l, code: c })
}

#[derive(Clone, Debug, PartialEq, Eq)]
pub enum Tok<'input> {
    // Keywords;
    Def,
    End,
    Extern,
    Forall,
    Fun,
    Inductive,
    In,
    Import,
    Let,
    Match,
    Module,
    Type,
    With,

    // Identifiers of various kinds:
    // SimpleName: String = {
    //     <s: r"[a-zA-Z_][a-zA-Z0-9_]*"> => s.to_string()
    // };
    Id(&'input str),
    StringLiteral(&'input str),
    // NumericLit(&'input str),

    Arrow,
    Bar,
    BangEquals,
    Colon,
    ColonColon,
    ColonEq,
    Comma,
    DotDot,
    Equals,
    EqualsEquals,
    EqualsGreaterThanCode(&'input str),
    EqualsGreaterThanQuestionCode(&'input str),
    EqualsGreaterThanLookahead,
    EqualsGreaterThanLookbehind,
    FatArrow,
    Hash,
    GreaterThan,
    LeftBrace,
    LeftBracket,
    LeftParen,
    LessThan,
    Lookahead, // @L
    Lookbehind, // @R
    Period,
    Plus,
    Question,
    RightBrace,
    RightBracket,
    RightParen,
    Semi,
    Star,
    TildeTilde,
    Underscore,
}

pub struct Tokenizer<'input> {
    text: &'input str,
    chars: CharIndices<'input>,
    lookahead: Option<(usize, char)>,
    shift: usize,
}

macro_rules! eof {
    ($x:expr) => {
        match $x { Some(v) => v, None => { return None; } }
    }
}

pub type Spanned<T> = (usize, T, usize);

const KEYWORDS: &'static [(&'static str, Tok<'static>)] = &[
    ("def", Def),
    ("end", End),
    ("extern", Extern),
    ("forall", Forall),
    ("fun", Fun),
    ("in", In),
    ("inductive", Inductive),
    ("import", Import),
    ("let", Let),
    ("match", Match),
    ("module", Module),
    ("Type", Type),
    ("with", With),
];

impl<'input> Tokenizer<'input> {
    pub fn new(text: &'input str, shift: usize) -> Tokenizer<'input> {
        let mut t = Tokenizer {
            text: text,
            chars: text.char_indices(),
            lookahead: None,
            shift: shift,
        };
        t.bump();
        t
    }

    fn next_unshifted(&mut self) -> Option<Result<Spanned<Tok<'input>>, Error>> {
        loop {
            return match self.lookahead {
                Some((idx0, '!')) => {
                    match self.bump() {
                        Some((idx1, '=')) => {
                            self.bump();
                            Some(Ok((idx0, BangEquals, idx1+1)))
                        }
                        _ => {
                            Some(error(UnrecognizedToken, idx0))
                        }
                    }
                }
                Some((idx0, ':')) => {
                    match self.bump() {
                        Some((idx1, ':')) => {
                            self.bump();
                            Some(Ok((idx0, ColonColon, idx1+1)))
                        }
                        Some((idx1, '=')) => {
                            self.bump();
                            Some(Ok((idx0, ColonEq, idx1+1)))
                        }
                        _ => {
                            Some(Ok((idx0, Colon, idx0+1)))
                        }
                    }
                }
                Some((idx0, ',')) => {
                    self.bump();
                    Some(Ok((idx0, Comma, idx0+1)))
                }
                Some((idx0, '.')) => {
                    match self.bump() {
                        Some((idx1, '.')) => {
                            self.bump();
                            Some(Ok((idx0, DotDot, idx1+1)))
                        }
                        _ => {
                            Some(Ok((idx0, Period, idx0+1)))
                        }
                    }
                }
                Some((idx0, '=')) => {
                    match self.bump() {
                        Some((idx1, '=')) => {
                            self.bump();
                            Some(Ok((idx0, EqualsEquals, idx1+1)))
                        }
                        Some((idx1, '>')) => {
                            self.bump();
                            Some(Ok((idx0, FatArrow, idx1+1)))
                        }
                        _ => {
                            Some(Ok((idx0, Equals, idx0+1)))
                        }
                    }
                }
                Some((idx0, '#')) => {
                    self.bump();
                    Some(Ok((idx0, Hash, idx0+1)))
                }
                Some((idx0, '>')) => {
                    self.bump();
                    Some(Ok((idx0, GreaterThan, idx0+1)))
                }
                Some((idx0, '{')) => {
                    self.bump();
                    Some(Ok((idx0, LeftBrace, idx0+1)))
                }
                Some((idx0, '[')) => {
                    self.bump();
                    Some(Ok((idx0, LeftBracket, idx0+1)))
                }
                Some((idx0, '(')) => {
                    self.bump();
                    Some(Ok((idx0, LeftParen, idx0+1)))
                }
                Some((idx0, '<')) => {
                    self.bump();
                    Some(Ok((idx0, LessThan, idx0+1)))
                }
                Some((idx0, '@')) => {
                    match self.bump() {
                        Some((idx1, 'L')) => {
                            self.bump();
                            Some(Ok((idx0, Lookahead, idx1+1)))
                        }
                        Some((idx1, 'R')) => {
                            self.bump();
                            Some(Ok((idx0, Lookbehind, idx1+1)))
                        }
                        _ => {
                            Some(error(UnrecognizedToken, idx0))
                        }
                    }
                }
                Some((idx0, '+')) => {
                    self.bump();
                    Some(Ok((idx0, Plus, idx0+1)))
                }
                Some((idx0, '-')) => {
                    match self.bump() {
                        Some((idx1, '>')) => {
                            self.bump();
                            Some(Ok((idx0, Arrow, idx1+1)))
                        }
                        Some((_, '-')) => {
                            match self.bump() {
                                Some((_, '|')) => {
                                    Some(self.doc_comment())
                                }
                                // This case makes it feel like we will need to move away
                                // from LALRPOP at some point towards a custom parser.
                                _ => {
                                    self.take_until(|c| c == '\n');
                                    continue;
                                }
                            }
                        }
                        _ => {
                            Some(error(UnrecognizedToken, idx0))
                        }
                    }
                }
                Some((idx0, '?')) => {
                    self.bump();
                    Some(Ok((idx0, Question, idx0+1)))
                }
                Some((idx0, '}')) => {
                    self.bump();
                    Some(Ok((idx0, RightBrace, idx0+1)))
                }
                Some((idx0, ']')) => {
                    self.bump();
                    Some(Ok((idx0, RightBracket, idx0+1)))
                }
                Some((idx0, ')')) => {
                    self.bump();
                    Some(Ok((idx0, RightParen, idx0+1)))
                }
                Some((idx0, ';')) => {
                    self.bump();
                    Some(Ok((idx0, Semi, idx0+1)))
                }
                Some((idx0, '*')) => {
                    self.bump();
                    Some(Ok((idx0, Star, idx0+1)))
                }
                Some((idx0, '~')) => {
                    match self.bump() {
                        Some((idx1, '~')) => {
                            self.bump();
                            Some(Ok((idx0, TildeTilde, idx1+1)))
                        }
                        _ => {
                            Some(error(UnrecognizedToken, idx0))
                        }
                    }
                }
                Some((idx0, '_')) => {
                    self.bump();
                    Some(Ok((idx0, Underscore, idx0+1)))
                }
                Some((idx0, '"')) => {
                    self.bump();
                    Some(self.string_literal(idx0))
                }
                Some((idx0, '|')) => {
                    self.bump();
                    Some(Ok((idx0, Bar, idx0+1)))
                }
                Some((idx0, c)) if is_identifier_start(c) => {
                    Some(self.identifierish(idx0))
                }
                Some((_, c)) if c.is_whitespace() => {
                    self.bump();
                    continue;
                }
                Some((idx, _)) => {
                    Some(error(UnrecognizedToken, idx))
                }
                None => {
                    None
                }
            };
        }
    }

    fn bump(&mut self) -> Option<(usize, char)> {
        self.lookahead = self.chars.next();
        self.lookahead
    }

    fn doc_comment(&mut self) -> Result<Spanned<Tok<'input>>, Error> {
        panic!()
    }

    fn string_literal(&mut self, idx0: usize) -> Result<Spanned<Tok<'input>>, Error> {
        let mut escape = false;
        let terminate = |c: char| {
            if escape {
                escape = false;
                false
            } else if c == '\\' {
                escape = true;
                false
            } else if c == '"' {
                true
            } else {
                false
            }
        };
        match self.take_until(terminate) {
            Some(idx1) => {
                self.bump(); // consume the '"'
                let text = &self.text[idx0+1..idx1]; // do not include the "" in the str
                Ok((idx0, StringLiteral(text), idx1+1))
            }
            None => {
                error(UnterminatedStringLiteral, idx0)
            }
        }
    }

    fn identifierish(&mut self, idx0: usize) -> Result<Spanned<Tok<'input>>, Error> {
        let (start, word, end) = self.word(idx0);

        let tok =
            KEYWORDS.iter()
                    .filter(|&&(w, _)| w == word)
                    .map(|&(_, ref t)| t.clone())
                    .next()
                    .unwrap_or_else(|| {
                        Id(word)
                    });

        Ok((start, tok, end))
    }

    fn word(&mut self, idx0: usize) -> Spanned<&'input str> {
        match self.take_while(is_identifier_continue) {
            Some(end) => (idx0, &self.text[idx0..end], end),
            None => (idx0, &self.text[idx0..], self.text.len()),
        }
    }

    fn take_while<F>(&mut self, mut keep_going: F) -> Option<usize>
        where F: FnMut(char) -> bool
    {
        self.take_until(|c| !keep_going(c))
    }

    fn take_until<F>(&mut self, mut terminate: F) -> Option<usize>
        where F: FnMut(char) -> bool
    {
        loop {
            match self.lookahead {
                None => {
                    return None;
                }
                Some((idx1, c)) => {
                    if terminate(c) {
                        return Some(idx1);
                    } else {
                        self.bump();
                    }
                }
            }
        }
    }
}

impl<'input> Iterator for Tokenizer<'input> {
    type Item = Result<Spanned<Tok<'input>>, Error>;

    fn next(&mut self) -> Option<Result<Spanned<Tok<'input>>, Error>> {
        match self.next_unshifted() {
            None =>
                None,
            Some(Ok((l, t, r))) =>
                Some(Ok((l+self.shift, t, r+self.shift))),
            Some(Err(Error { location, code })) =>
                Some(Err(Error { location: location+self.shift, code: code })),
        }
    }
}

fn is_identifier_start(c: char) -> bool {
    UnicodeXID::is_xid_start(c)
}

fn is_identifier_continue(c: char) -> bool {
    UnicodeXID::is_xid_continue(c)
}
