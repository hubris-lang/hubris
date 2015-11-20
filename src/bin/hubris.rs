#[macro_use]
extern crate log;
extern crate hubris;
extern crate env_logger;

use std::env;

fn main() {
    env_logger::init().unwrap();
    let mut args = env::args();
    args.next();
    hubris::compile_file(&args.next().unwrap()[..]);
}
