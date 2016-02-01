use super::elaborate::{self, ElabCx, LocalElabCx};
use super::parser;
use super::core;
use ast::{self, SourceMap};
use super::typeck::{self, LocalCx};

use std::io;
use std::path::PathBuf;
use readline;

const HELP_MESSAGE: &'static str = r#"
Commands:
    :help        Show this message
    :type <term> Infer the type of <term>
    :quit        Exit
"#;

pub struct Repl {
    elab_cx: ElabCx,
    root_file: Option<PathBuf>,
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

impl Repl {
    pub fn from_path(file: &Option<PathBuf>) -> Result<Repl, Error> {
        match file {
            &None => {
                let ecx = ElabCx::from_module(ast::Module::empty(), SourceMap::empty());
                Ok(Repl {
                    elab_cx: ecx,
                    root_file: None
                })
            }
            &Some(ref file_path) => {
                let parser = try!(parser::from_file(file_path));
                let module = try!(parser.parse());

                let mut ecx = ElabCx::from_module(module, parser.source_map.clone());

                let emodule = try!(ecx.elaborate_module(file_path));

                {
                    let main = try!(ecx.ty_cx.get_main_body());
                    let result = try!(ecx.ty_cx.eval(main));
                    println!("main={}", result);
                }

                Ok(Repl {
                    elab_cx: ecx,
                    root_file: file.clone(),
                })
            }
        }
    }

    /// Starts the read-eval-print-loop for querying the language.
    pub fn start(mut self) -> Result<(), Error> {
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
                Err(e) => println!("repl error: {:?}", e),
                Ok(cont) => {
                    match cont {
                        Cont::Quit => break,
                        Cont::Done => {}
                    }
                }
            }
        }

        Ok(())
    }

    pub fn repl_interation(&mut self, input: String) -> Result<Cont, Error> {
        if input.len() <= 0 {
            // do nothing
        } else if &input[0..1] == ":" {
            let cmd = self.parse_command(&input[1..]);
            match cmd {
                Command::Quit => return Ok(Cont::Quit),
                Command::Reload => panic!("unsupported command"),
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
        let mut ltycx = LocalCx::from_cx(&self.elab_cx.ty_cx);
        let term = try!(ltycx.type_infer_term(&term));

        Ok(term)
    }

    fn handle_input(&mut self, source: String) -> Result<(), Error> {
        let term = try!(self.preprocess_term(source));
        let ty = try!(self.type_check_term(&term));
        println!("{} : {}", term, ty);
        println!("{}", try!(self.elab_cx.ty_cx.eval(&term)));
        Ok(())
    }

    fn parse_command(&self, command_text: &str) -> Command {
        if command_text == "quit" {
            Command::Quit
        } else if command_text == "reload" {
            Command::Reload
        } else if &command_text[0..4] == "type" || &command_text[0..1] == "t" {
            Command::TypeOf(command_text[4..].to_string())
        } else if &command_text[0..4] == "help" {
            Command::Help
        } else {
            Command::Unknown(command_text.to_string())
        }
    }
}
