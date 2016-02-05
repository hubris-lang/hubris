use super::core;
use super::elaborate::{self, ElabCx, LocalElabCx};
use super::error_reporting::{ErrorContext, Report};
use super::parser;
use super::ast::{self, SourceMap};
use super::typeck;

use std::io::{self, Write};
use std::path::{PathBuf};
use readline;

use term::{self, Terminal, StdoutTerminal, Result as TResult};

const HELP_MESSAGE: &'static str = r#"
Commands:
    :help        Show this message
    :type <term> Infer the type of <term>
    :reload      Reload the session
    :quit        Exit
"#;

pub struct Repl {
    elab_cx: ElabCx,
    root_file: Option<PathBuf>,
    terminal: Box<StdoutTerminal>,
}

// impl From<parser::Error> for Error {
//     fn from(err: parser::Error) -> Error {
//         Error::Parser(err)
//     }
// }
//

impl From<io::Error> for Error {
    fn from(err: io::Error) -> Error {
        Error::Io(err)
    }
}

impl From<elaborate::Error> for Error {
    fn from(err: elaborate::Error) -> Error {
        Error::Elaborator(err)
    }
}

impl From<typeck::Error> for Error {
    fn from(err: typeck::Error) -> Error {
        Error::TypeCk(err)
    }
}

impl From<term::Error> for Error {
    fn from(err: term::Error) -> Error {
        Error::Term(err)
    }
}

impl From<parser::Error> for Error {
    fn from(err: parser::Error) -> Error {
        Error::Parser(err)
    }
}

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Elaborator(elaborate::Error),
    UnknownCommand(String),
    TypeCk(typeck::Error),
    Parser(parser::Error),
    Term(term::Error),
}

#[derive(Debug)]
enum Command {
    Quit,
    Reload,
    Unknown(String),
    TypeOf(String),
    Help,
}

pub enum Cont {
    Quit,
    Done,
}

fn split_command(command_text: &str) -> (&str, &str) {

    for (i, c) in command_text.char_indices() {
        if c.is_whitespace() {
            return command_text.split_at(i);
        }
    }

    (command_text, "")
}

impl Repl {
    pub fn from_path(file: &Option<PathBuf>) -> Result<Repl, Error> {
        match file {
            &None => {
                let ecx = ElabCx::from_module(ast::Module::empty(), SourceMap::empty());
                Ok(Repl {
                    elab_cx: ecx,
                    root_file: None,
                    terminal: term::stdout().unwrap(),
                })
            }
            &Some(ref file_path) => {
                let parser = try!(parser::from_file(file_path));
                let module = try!(parser.parse());

                let mut ecx = ElabCx::from_module(module, parser.source_map.clone());

                // Ensure that if a type error occurs here we report it, ideally
                // the REPL should launch anyways.
                match ecx.elaborate_module(file_path) {
                    Err(e) => { try!(e.report(&mut ecx)) },
                    Ok(_) => {}
                }

                Ok(Repl {
                    elab_cx: ecx,
                    root_file: file.clone(),
                    terminal: term::stdout().unwrap(),
                })
            }
        }
    }

    /// Starts the read-eval-print-loop for querying the language.
    pub fn start(mut self) -> Result<(), Error> {
        readline::read_history(&PathBuf::from("~/.hubris_history"));

        loop {
            // First we grab a line ...
            let input = match readline::readline("hubris> ") {
                None => {
                    println!("");
                    break;
                }
                Some(input) => input,
            };

            // Add it to the history
            readline::add_history(input.as_ref());

            match self.repl_interation(input) {
                // please make me look better
                Err(e) => {
                    try!(e.report(&mut self));
                },
                Ok(cont) => {
                    match cont {
                        Cont::Quit => break,
                        Cont::Done => {}
                    }
                }
            }
        }

        readline::write_history(&PathBuf::from("~/.hubris_history"));

        Ok(())
    }

    pub fn repl_interation(&mut self, input: String) -> Result<Cont, Error> {
        if input.len() <= 0 {
            // do nothing
        } else if &input[0..1] == ":" {
            let cmd = self.parse_command(&input[1..]);
            match cmd {
                Command::Quit => return Ok(Cont::Quit),
                Command::Reload => {
                    let new_repl =
                        try!(Repl::from_path(&self.root_file));
                    *self = new_repl;
                }
                Command::Unknown(u) => return Err(Error::UnknownCommand(u)),
                Command::TypeOf(t) => {
                    let term = try!(self.preprocess_term(t));
                    println!("{}", try!(self.type_check_term(&term)));
                }
                Command::Help => println!("{}", HELP_MESSAGE),
            }
        } else {
            try!(self.handle_input(input.to_string()));
        }

        Ok(Cont::Done)
    }

    fn preprocess_term(&mut self, source: String) -> Result<core::Term, Error> {
        let parser = parser::from_string(source).unwrap();
        let term = try!(parser.parse_term());

        let mut lcx = LocalElabCx::from_elab_cx(&mut self.elab_cx);
        let term = try!(lcx.elaborate_term(term));

        Ok(term)
    }

    fn type_check_term(&mut self, term: &core::Term) -> Result<core::Term, Error> {
        let term = try!(self.elab_cx.ty_cx.type_infer_term(&term));
        let ty = try!(self.elab_cx.ty_cx.eval(&term));
        Ok(ty)
    }

    fn handle_input(&mut self, source: String) -> Result<(), Error> {
        let term = try!(self.preprocess_term(source));
        let ty = try!(self.type_check_term(&term));
        println!("{} : {}", term, ty);
        println!("{}", try!(self.elab_cx.ty_cx.eval(&term)));
        Ok(())
    }

    fn parse_command(&self, command_text: &str) -> Command {
        let (command, arg) = split_command(command_text);

        if command == "" {
            Command::Unknown(command_text.to_string())
        } else if "quit".starts_with(command) {
            Command::Quit
        } else if "reload".starts_with(command) {
            Command::Reload
        } else if "type".starts_with(command) {
            Command::TypeOf(arg.to_string())
        } else if "help".starts_with(command) {
            Command::Help
        } else {
            Command::Unknown(command_text.to_string())
        }
    }
}

impl ErrorContext<io::Stdout> for Repl {
    fn get_source_map(&self) -> &SourceMap {
        &self.elab_cx.ty_cx.source_map
    }

    fn get_terminal(&mut self) -> &mut Box<Terminal<Output=io::Stdout> + Send> {
        &mut self.elab_cx.ty_cx.terminal
    }
}

impl<O: Write, E: ErrorContext<O>> Report<O, E> for Error {
    fn report(self, cx: &mut E) -> TResult<()> {
        match self {
            Error::TypeCk(ty_ck_err) => {
                ty_ck_err.report(cx)
            }
            Error::Elaborator(elab_err) => {
                elab_err.report(cx)
            }
            Error::Parser(parser_err) => {
                parser_err.report(cx)
            }
            e => panic!("need to support better error printing for this {:?}", e),
        }
    }
}
