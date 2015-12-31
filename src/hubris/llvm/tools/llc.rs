use std::process::Command;
use std::fmt::Debug;
use std::path::Path;

pub fn llc<P: AsRef<Path> + Debug>(file: P) {
    debug!("executing llc on {:?}", file);
    Command::new("llc")
           .arg(file.as_ref())
           .output()
           .unwrap_or_else(|e| {
               panic!("llc: failed with {:?}", e)
           });
}
