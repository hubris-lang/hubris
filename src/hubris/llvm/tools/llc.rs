use std::process;
use std::fmt::Debug;
use std::path::Path;

pub fn llc<P: AsRef<Path> + Debug>(file: P) {
    println!("running llc on {:?}", file);
}
