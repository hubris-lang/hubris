#[macro_use]
extern crate log;
extern crate hubris;
extern crate env_logger;
extern crate rustc_serialize;
extern crate docopt;

use docopt::Docopt;
use std::path::PathBuf;
use std::process;
use std::io;

use hubris::session::{Session, HasSession, Reportable};

const USAGE: &'static str = r#"
Hubris, version 0.0.1.

  ___ ___      ___.         .__
 /   |   \ __ _\_ |_________|__| ______
/    ~    \  |  \ __ \_  __ \  |/  ___/
\    Y    /  |  / \_\ \  | \/  |\___ \
 \___|_  /|____/|___  /__|  |__/____  >
       \/           \/              \/

Usage:
    hubris repl [<file>]
    hubris server
    hubris <file> [--output=<exe> --log=<logfile>]
    hubris (-h | --help)
    hubris --version

Options:
    -h --help    Show this screen.
    --version    Show version.
"#;

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_file: Option<String>,
    flag_output: Option<String>,
    flag_logging: Option<String>,
    flag_version: bool,
    cmd_server: bool,
    cmd_repl: bool,
}

fn main() {
    // TODO: add logger flags to redirect to a fd
    env_logger::init().unwrap();

    let args: Args = Docopt::new(USAGE)
                         .and_then(|d| d.decode())
                         .unwrap_or_else(|e| e.exit());

    driver(args).unwrap();
}

fn driver(args: Args) -> io::Result<()> {
    let session = args.arg_file.clone().map(|file_path| {
        let file_path = PathBuf::from(file_path);
        if !file_path.is_file() {
                println!("hubris: file {} does not exist", file_path.display());
                process::exit(1);
        }
        Session::from_root(&file_path)
    }).unwrap_or(Session::empty());

    if args.flag_version {
        println!("hubris 0.1.0");
    } else if args.cmd_server {
        println!("Starting Server...");
        hubris::server::run();
    } else if args.cmd_repl {
        match hubris::repl::Repl::from_session(session.clone()) {
            Err(e) => session.report(e).unwrap(),
            Ok(repl) => match repl.start() {
                Err(e) => session.report(e).unwrap(),
                Ok(_) => {}
            }
        }
    } else {
        let input = match args.arg_file {
            None => {
                println!("hubris: no input files");
                return Ok(());
            }
            Some(file) => file,
        };

        debug!("main: compiling {} output to {:?}",
               &input[..],
               args.flag_output);

        let result = hubris::compile_file(&input[..],
                                          args.flag_output.map(|p| PathBuf::from(p)));

        match result {
            Err(e) => try!(session.report(e)),
            Ok(_) => {}
        }
    }

    Ok(())
}
