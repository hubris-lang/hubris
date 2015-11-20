extern crate hubris;

use std::env;

fn main() {
    let mut args = env::args();
    args.next();
    hubris::compile_file(&args.next().unwrap()[..]);
}
