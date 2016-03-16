use std::collections::HashMap;
use std::env;
use std::fmt::Debug;
use std::fs::{File};
use std::io;
use std::io::Write;
use std::process::Command;
use std::path::{Path, PathBuf};

use core;
use llvm;
use llvm_sys;

use llvm_sys::prelude::LLVMValueRef;

#[derive(Debug, Clone)]
enum Error {
    UnknownSymbol(String),
}

type Module = ();

/// A trait that describes the interface to a particular compiler backend.
trait Backend {
    fn create_executable<P: AsRef<Path> + Debug>(module: &Module, output: Option<P>);
}
