use std::env;
use std::fmt::Debug;
use std::fs::File;
use std::io::Write;
use std::process::Command;
use std::path::Path;

use ast;
use llvm;

pub fn emit_module<P: AsRef<Path> + Debug>(module: &ast::Module, output: P) {
    llvm::generate_ir(module.name.clone(), output.as_ref());
    llvm::tools::llc(output);
}

pub fn in_build_path<P: AsRef<Path>, F, R>(build_path: P, f: F) -> R
    where F: Fn(&Path) -> R {
    let current_dir = env::current_dir().unwrap();
    env::set_current_dir(build_path.as_ref());
    let result = f(build_path.as_ref());
    env::set_current_dir(current_dir);
    result
}

pub fn create_executable(module: &ast::Module, output: Option<&Path>) {
    let exe = in_build_path("/tmp", |build_path| {
        let file_name = module.file_name();

        emit_module(
            &module,
            build_path.join(file_name.with_extension("ll")));

            Command::new("as")
                   .arg(file_name.with_extension("s"))
                   .arg("-o")
                   .arg(file_name.with_extension("o"))
                   .output()
                   .unwrap_or_else(|_| panic!("as failed"));

            let rt = include_str!("../../rt.c");

            let mut rtc = File::create(build_path.join("rt.c")).unwrap();
            rtc.write_all(rt.as_bytes()).unwrap();

            Command::new("gcc")
                    .arg("rt.c")
                    .arg(file_name.with_extension("o"))
                    .arg("-o")
                    .arg("prog")
                    .output()
                    .unwrap_or_else(|_| panic!("gcc failed"));

            build_path.join("prog")
    });

    let current_dir = env::current_dir().unwrap();
    let output = output.unwrap_or(current_dir.as_ref());

    Command::new("cp")
           .arg(exe)
           .arg(output)
           .output()
           .unwrap_or_else(|_| panic!("cp failed"));
}
