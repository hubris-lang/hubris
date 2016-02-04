#[macro_use]
extern crate log;
extern crate hubris;
extern crate env_logger;
extern crate rustc_serialize;
extern crate docopt;

use docopt::Docopt;
use std::path::PathBuf;

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
    hubris <file> [--output=<exe>]
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

    if args.flag_version {
        println!("hubris 0.0.1.0 (we are trying)");
    } else if args.cmd_server {
        println!("Starting Server...");
        hubris::server::server_main();
    } else if args.cmd_repl {
        // verify file exists
        let pb = args.arg_file.map(|f| {
            let pb = PathBuf::from(f);
            if !pb.is_file() {
                panic!("Hubris: file {} does not exist", pb.to_str().unwrap());
            }
            pb
        });

        let repl = hubris::repl::Repl::from_path(&pb).expect("Launching repl failed");
        repl.start().expect("Starting repl failed");
    } else {
        let input = args.arg_file.expect("No input files");
        
        debug!("main: compiling {} output to {:?}",
               &input[..],
               args.flag_output);

        let result = hubris::compile_file(&input[..],
                                          args.flag_output.map(|p| PathBuf::from(p)));

        match result {
            Err(e) => panic!("failed with {:?}", e),
            Ok(_) => {}
        }
    }
}
