use super::elaborate::{self, ElabCx};
use super::parser;
use super::typeck::TyCtxt;

use std::io;
use std::path::{PathBuf};

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
        panic!("start")
        // for line in self.readline_session.clone() {
        //     if line.starts_with(":") {
        //         let command = self.parse_command(&line[1..]);
        //         match command {
        //             Command::Quit => break,
        //             Command::Reload => {
        //                 match self.file.as_ref() {
        //                     None => println!("no file to reload"),
        //                     Some(file) => {
        //                         self.interpreter = try!(Interpreter::load_from_file(file));
        //                     }
        //                 }
        //             },
        //             _ => println!("unrecognized command {}", &line[1..])
        //         }
        //     } else {
        //         try!(self.query(line))
        //     }
        // }
        //
        // Ok(())
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

// fn query(&mut self, query: String) -> Result<(), Error>  {
//     let query_parser = Parser::new(query);
//     let goal = try!(query_parser.parse_query());
//     debug!("Repl::query: query_string={:?}", goal);
//     let mut session  = ReadlineSession::new("more? ".to_owned());
//     try!(self.interpreter.solve(goal, || {
//         let line = session.next().unwrap();
//         println!("Inside the callback !!: {}", line);
//         let line = session.next().unwrap();
//
//         if &line[..1] == ";" {
//             Ok(())
//         } else {
//             Err(())
//         }
//     }));
//     // for var in vars {
//     //     let value = self.environment
//     //     println!("{} = {}", var, );
//     // }
//     Ok(())
// }
//
