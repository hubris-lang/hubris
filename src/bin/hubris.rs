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
    hubris <file> [--output=<exe>]
    hubris (-r | --interactive) <file>
    hubris (-s | --server)
    hubris (-h | --help)
    hubris --version

Options:
    -s --server  Launch compiler in server mode.
    -h --help    Show this screen.
    --version    Show version.
"#;

#[derive(Debug, RustcDecodable)]
struct Args {
    arg_file: String,
    flag_output: Option<String>,
    flag_server: bool,
    flag_interactive: bool,
}

fn main() {
    env_logger::init().unwrap();

    let args: Args = Docopt::new(USAGE)
                         .and_then(|d| d.decode())
                         .unwrap_or_else(|e| e.exit());

    if args.flag_server {
        hubris::server::server_main();
    } else if args.flag_interactive {
        let repl = hubris::repl::Repl::from_path(&Some(PathBuf::from(args.arg_file))).unwrap();
        repl.start().unwrap();
    } else {
        debug!("main: compiling {} output to {:?}",
               &args.arg_file[..],
               args.arg_file);

        let result = hubris::compile_file(&args.arg_file[..],
                                          args.flag_output.map(|p| PathBuf::from(p)));

        match result {
            Err(e) => panic!("failed with {:?}", e),
            Ok(_) => {}
        }
    }
}
