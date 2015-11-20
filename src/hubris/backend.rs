use std::process::Command;
use std::path::Path;
use std::fmt::Debug;

use ast;
use llvm;

pub fn emit_module<P: AsRef<Path> + Debug>(module: ast::Module, output: P) {
    llvm::generate_ir(module.name, output.as_ref());
    llvm::tools::llc(output);
}
