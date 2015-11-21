use std::env;
use std::fmt::Debug;
use std::fs::{File};
use std::io::Write;
use std::process::Command;
use std::path::{Path, PathBuf};

use ast;
use llvm;
use llvm_sys;

pub fn emit_module<P: AsRef<Path> + Debug>(m: &ast::Module, output: P) {
    let mut context = llvm::Context::new();
    let module = llvm::Module::with_name(m.name.clone());
    module.set_target("x86_64-apple-darwin".to_string());

    for def in &m.defs {
        match def {
            &ast::Definition::Fn(ref fun) => emit_function(&mut context, &module, fun),
            &ast::Definition::Extern(ref n, ref ty) => emit_extern(&mut context, &module, n, ty),
            _ => {}
        }
    }

    module.dump();
    module.print_to_file(output);
}

pub fn emit_function(cx: &mut llvm::Context,
                     module: &llvm::Module,
                     func: &ast::Function) {
    let builder = llvm::Builder::in_context(cx);

    let fn_ty = llvm::FunctionType::new(cx.void_type(), vec![]);

    let fname = if func.name == "main" {
        "_hubris_main".to_string()
    } else {
        func.name.clone()
    };

    let function = llvm::Function::in_module(
        &module,
        fname,
        fn_ty.clone());

    let bb = function.append_bb_in_ctxt(&cx, "entry".to_string());

    builder.postition_at_end(bb);

    let callee = module.get_function("hello_world".to_string());
    builder.emit_call(&callee, vec![]);

    // Emit a `ret void` into the function
    builder.emit_ret_void();
}

pub fn emit_extern(cx: &llvm::Context,
                   module: &llvm::Module,
                   name: &ast::Name,
                   ty: &ast::Type) {
    let mut context = llvm::Context::new();
    let builder = llvm::Builder::in_context(&mut context);

    let fn_ty = llvm::FunctionType::new(context.void_type(), vec![]);

    let efunction = llvm::Function::in_module(
        &module,
        name.clone(),
        fn_ty);

    efunction.set_linkage(llvm_sys::LLVMLinkage::LLVMExternalLinkage);
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
    let current_dir = env::current_dir().unwrap();
    let output = output.unwrap_or(current_dir.as_ref());
    let tmp = PathBuf::from("/tmp");

    Command::new("mkdir")
           .arg("-p")
           .arg("/tmp/hubris")
           .output()
           .unwrap_or_else(|_| panic!("as failed"));

    let exe = in_build_path(tmp.join("hubris"), |build_path| {
        let file_name = module.file_name();

        emit_module(
            &module,
            build_path.join(file_name.with_extension("ll")));

        llvm::tools::llc(output);

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

    Command::new("cp")
           .arg(exe)
           .arg(output)
           .output()
           .unwrap_or_else(|_| panic!("cp failed"));
}
