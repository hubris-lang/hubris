use super::core;
use super::elaborate::{self, ElabCx, LocalElabCx};
use super::parser;
use super::session::{Session, Reportable, HasSession};
use super::ast::{self, ModuleId, SourceMap};
use super::typeck;

use std::io::{self, stdout};
use std::path::{PathBuf};
use readline;

use super::pretty::*;

use term;

const HELP_MESSAGE: &'static str = r#"
Commands:
    :help        Show this message
    :type <term> Infer the type of <term>
    :reload      Reload the session
    :def         Print a debug representation of a term
    :quit        Exit
"#;

pub struct Repl {
    elab_cx: ElabCx,
    session: Session,
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
    Def(String),
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
    pub fn from_session(session: Session) -> Result<Repl, Error> {
        let id = session.next_module_id();

        let mut ecx = if !session.root_file().is_dir() {
            let parser =
                try!(parser::from_file(session.root_file(), id));

            let module = try!(parser.parse());

            session.add_source_map_for(id, parser.source_map);

            ElabCx::from_module(
                module,
                session.clone())
        } else {
            ElabCx::from_module(
                ast::Module::empty(),
                session.clone())
        };

        // Ensure that if a type error occurs here we report it, ideally
        // the REPL should launch anyways.
        match ecx.elaborate_module() {
            Err(e) => { try!(ecx.report(e)) },
            Ok(_) => {}
        }

        Ok(Repl {
            elab_cx: ecx,
            session: session,
        })
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
                    try!(self.report(e));
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
                        try!(Repl::from_session(self.session.clone()));
                    *self = new_repl;
                }
                Command::Unknown(u) => return Err(Error::UnknownCommand(u)),
                Command::TypeOf(t) => {
                    let term = try!(self.preprocess_term(t));
                    let typed = try!(self.type_check_term(&term)).1;
                    println!("{}", typed)
                }
                Command::Def(name) => {
                    let parser = parser::from_string(name, ast::ModuleId(0)).unwrap();
                    let name = try!(parser.parse_name());

                    // Not really sure why I put this code here ...
                    //
                    // match &mut self.elab_cx.ty_cx.session.ty {
                    //     &mut SessionType::Repl { ref mut source_map, .. } =>
                    //         *source_map = parser.source_map,
                    //     _ => panic!()
                    // }

                    let name = try!(self.elab_cx.elaborate_global_name(name));

                    match self.elab_cx.ty_cx.unfold_name(&name).ok() {
                        None => println!("could not find a definition for {}", name),
                        Some(t) => {
                            try!(Doc::render(&t.pretty(), 80, &mut stdout()))
                        }
                    }
                }
                Command::Help => println!("{}", HELP_MESSAGE),
                // Command::Debug =>
            }
        } else {
            try!(self.handle_input(input.to_string()));
        }

        Ok(Cont::Done)
    }

    fn preprocess_term(&mut self, source: String) -> Result<core::Term, Error> {
        let source_copy = source.clone();

        self.session.add_source_map_for(ModuleId(0), SourceMap::from_source(source));

        let parser = parser::from_string(source_copy, ModuleId(0)).unwrap();
        let term = try!(parser.parse_term());

        let mut lcx = LocalElabCx::from_elab_cx(&mut self.elab_cx);
        let term = try!(lcx.elaborate_term(term));

        Ok(term)
    }

    fn type_check_term(&mut self, term: &core::Term) -> Result<(core::Term, core::Term), Error> {
        let (term, ty) = try!(self.elab_cx.ty_cx.type_check_term(&term, None));
        let ty = try!(self.elab_cx.ty_cx.eval(&ty));

        Ok((term, ty))
    }

    fn handle_input(&mut self, source: String) -> Result<(), Error> {
        let term = try!(self.preprocess_term(source));
        let (term, ty) = try!(self.type_check_term(&term));
        // println!("{} : {}", term, ty);
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
        } else if "def".starts_with(command) {
            Command::Def(arg.to_string())
        } else {
            Command::Unknown(command_text.to_string())
        }
    }
}

impl HasSession for Repl {
    fn session(&self) -> &Session {
        &self.session
    }
}

impl Reportable for Error {
    fn report(self, session: &Session) -> io::Result<()> {
        match self {
            Error::TypeCk(ty_ck_err) => {
                ty_ck_err.report(session)
            }
            Error::Elaborator(elab_err) => {
                elab_err.report(session)
            }
            Error::Parser(parser_err) => {
                parser_err.report(session)
            }
            Error::UnknownCommand(cmd) => {
                println!("Unknown command \"{}\"", cmd);
                Ok(())
            }
            e => panic!("need to support better error printing for this {:?}", e),
        }
    }
}
