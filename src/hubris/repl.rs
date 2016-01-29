use super::elaborate::{self, ElabCx, LocalElabCx};
use super::parser;
use super::typeck::{TyCtxt, LocalCx};

use std::io;
use std::path::{PathBuf};
use readline;

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

#[derive(Debug)]
pub enum Error {
    Io(io::Error),
    Elaborator(elaborate::Error),
    // Parser(parser::Error),
    // Interpreter(interpreter::Error),
}

#[derive(Debug)]
enum Command {
    Quit,
    Reload,
    Unknown,
}

impl Repl {
    pub fn from_path(file: &Option<PathBuf>) -> Result<Repl, Error> {
        match file {
            &None => panic!("need to load the REPL with a file"),
            &Some(ref file_path) => {
                let parser = try!(parser::from_file(file_path));
                let module = parser.parse();

                let mut ecx = ElabCx::from_module(
                    module,
                    parser.source_map.clone());

                let emodule = try!(ecx.elaborate_module(
                    file_path));

                let ty_cx = TyCtxt::from_module(&emodule, parser.source_map);

                // let term = term::stdout().unwrap();

                for def in &emodule.defs {
                    match ty_cx.type_check_def(def) {
                        Err(err) => {
                            panic!("error {:?}", err);
                            //report_type_error(&ty_cx, term, err).unwrap(); // handle this properly
                            // return Ok(());
                        }
                        Ok(_) => {
                            let t = ty_cx.get_main_body();
                            println!("main={}",
                                ty_cx.eval(t).unwrap());
                        }
                    }
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
            let input = match readline::readline("hubris> ") {
                None => {
                    println!("");
                    break;
                },
                Some(input) => input,
            };

            if &input[0..1] == ":" {
                let cmd = self.parse_command(&input[1..]);
                match cmd {
                    Command::Quit => return Ok(()),
                    Command::Reload => panic!("unsupported command"),
                    Command::Unknown => panic!("unknown name"),
                }
            } else {
                self.check_term(input.to_string());
                readline::add_history(input.as_ref());
            }
        }

        Ok(())
    }

    fn check_term(&mut self, source: String) {
        let parser = parser::from_string(source).unwrap();
        let term = parser.parse_term();

        let eterm = {
            let mut lcx = LocalElabCx::from_elab_cx(&mut self.elab_cx);
            lcx.elaborate_term(term).unwrap()
        };
        
        let mut ltycx = LocalCx::from_cx(&self.elab_cx.ty_cx);
        let result = ltycx.type_infer_term(&eterm).unwrap();
        println!("{} : {}", eterm, result);
    }

    fn parse_command(&self, command_text: &str) -> Command {
        if command_text == "quit" {
            Command::Quit
        } else if command_text == "reload" {
            Command::Reload
        } else {
            Command::Unknown
        }
    }
}
//
