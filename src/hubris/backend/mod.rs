use std::env;
use std::fmt::Debug;
use std::fs::{File};
use std::io;
use std::io::Write;
use std::process::Command;
use std::path::{Path, PathBuf};

use core;
use cps;
use llvm;
use llvm_sys;

use llvm_sys::prelude::LLVMValueRef;

enum CodegenErr {
    Err
}

struct ModuleCx<'cx, 'm> {
    cx: &'cx llvm::Context,
    module: llvm::Module,
    cps_module: &'m cps::Module,
}

impl<'cx, 'm> ModuleCx<'cx, 'm> {
    fn new(cx: &'cx llvm::Context, m: &'m cps::Module) -> ModuleCx<'cx, 'm> {
        let module = llvm::Module::with_name(m.name.clone());
        module.set_target("x86_64-apple-darwin".to_string());

        ModuleCx {
            cx: cx,
            module: module,
            cps_module: m,
        }
    }

    pub fn emit_module<P: AsRef<Path> + Debug>(&self, output: P) {
        for def in &self.cps_module.defs {
            match def {
                &cps::Definition::Fn(ref fun) => {
                    let fcx = FunctionCx::new(self, fun);
                    fcx.emit_function();
                }
        //        &core::Definition::Extern(ref e) => emit_extern(e),
                _ => {}
            }
        }

        self.module.dump();
        self.module.print_to_file(output);
    }
}

struct FunctionCx<'cx, 'm:'cx, 'f> {
    mcx: &'cx ModuleCx<'cx, 'm>,
    func: &'f cps::Function,
    builder: llvm::Builder,
}

impl<'cx, 'm, 'f> FunctionCx<'cx, 'm, 'f> {
    fn new(module: &'cx ModuleCx<'cx, 'm>, func: &'f cps::Function) -> FunctionCx<'cx, 'm, 'f> {
        FunctionCx {
            mcx: module,
            func: func,
            builder: llvm::Builder::in_context(module.cx)
        }
    }

    pub fn emit_function(&self) -> Result<(), CodegenErr> {
        use core::{Term, Literal};

        let fn_ty = llvm::FunctionType::new(self.mcx.cx.void_type(), vec![]);

        let fname = if self.func.name == "main" {
            "_hubris_main".to_string()
        } else {
            self.func.name.clone()
        };

        let function = llvm::Function::in_module(
            &self.mcx.module,
            fname,
            fn_ty.clone());

        let bb = function.append_bb_in_ctxt(&self.mcx.cx, "entry".to_string());
        self.builder.postition_at_end(bb);

        let return_value = self.emit_term(&self.func.body);

        self.builder.emit_ret_void();

        // match self.func.ty {
        //     Term::Literal(Literal::Unit) => { self.builder.emit_ret_void(); },
        //     _ => panic!("return type doesn't work"),
        // }

        Ok(())
    }

    pub fn emit_term(&self, term: &cps::Term) -> Result<LLVMValueRef, CodegenErr> {
        panic!()
        // use core::Term::*;
        //
        // match term {
        //     &Literal(ref l) => panic!(),
        //     &Var(ref v) => {
        //         Ok(unsafe { self.mcx.module.get_function(v.clone()).as_ptr() })
        //     }
        //     &Match(ref scrutinee, ref cases) => { panic!("can't emit match") }
        //     &App(ref fun, ref arg) => {
        //         let callee = self.emit_term(&**fun);
        //
        //                 // let function = match self.module.get_function(name) {
        //                 //     Some(function) => function,
        //                 //     None => return error("unknown function referenced")
        //                 // };
        //                 //
        //                 // let arg = try!(arg.codegen(context, module_provider));
        //                 //
        //                 // Ok((context.builder.build_call(function.to_ref(),
        //                 //                                [arg].as_mut_slice(),
        //                 //                                "calltmp"),
        //                 //        false))
        //
        //         panic!() // fix me Ok(self.builder.emit_call(&callee, vec![]))
        //     }
        //     &Type => {
        //         panic!("can't code gen Type")
        //     }
        //     &Forall(..) => {
        //         panic!("should not be trying to codegen a forall")
        //     }
        //     &Lambda(..) => {
        //         panic!("can't codegen lambdas")
        //     }
        //     &Metavar(_) => {
        //         panic!("can't codegen metavars")
        //     }
        // }
    }

    pub fn emit_literal(&self, lit: &core::Literal) {}

    pub fn emit_extern(&self, name: &core::Name, ty: &core::Term) {
        let mut context = llvm::Context::new();
        let builder = llvm::Builder::in_context(&mut context);

        let fn_ty = llvm::FunctionType::new(context.void_type(), vec![]);

        let efunction = llvm::Function::in_module(
            &self.mcx.module,
            name.clone(),
            fn_ty);

        efunction.set_linkage(llvm_sys::LLVMLinkage::LLVMExternalLinkage);
    }
}

pub fn in_build_path<P: AsRef<Path>, F, R>(build_path: P, f: F) -> R
    where F: Fn(&Path) -> R {

    let current_dir = env::current_dir().unwrap();
    env::set_current_dir(build_path.as_ref());
    let result = f(build_path.as_ref());
    env::set_current_dir(current_dir);
    result
}

pub fn ensure_success(cmd: &mut Command) -> io::Result<()> {
    let output = try!(cmd.output());
    if !output.status.success() {
        panic!("command {:?} failed with: {:?}", cmd, output.stdout);
    } else {
        Ok(())
    }
}

pub fn create_executable<P: AsRef<Path> + Debug>(module: &cps::Module, output: Option<P>) {
    let current_dir = env::current_dir().unwrap();
    let output = output.as_ref().map(|x| x.as_ref()).unwrap_or(current_dir.as_ref());
    let tmp = PathBuf::from("/tmp");

    ensure_success(
        Command::new("mkdir")
           .arg("-p")
           .arg("/tmp/hubris"));

    let exe = in_build_path(tmp.join("hubris"), |build_path| {
        let file_name = module.file_name();

        let mut context = llvm::Context::new();
        let mcx = ModuleCx::new(&context, module);

        mcx.emit_module(
            build_path.join(file_name.with_extension("ll")));

        llvm::tools::llc(
            build_path.join(file_name.with_extension("ll")));

        ensure_success(
            Command::new("as")
               .arg(file_name.with_extension("s"))
               .arg("-o")
               .arg(file_name.with_extension("o")));

        let rt = include_str!("../../../rt.c");

        let mut rtc = File::create(build_path.join("rt.c")).unwrap();
        rtc.write_all(rt.as_bytes()).unwrap();

        ensure_success(
            Command::new("gcc")
                .arg("rt.c")
                .arg(file_name.with_extension("o"))
                .arg("-o")
                .arg("prog"));

        build_path.join("prog")
    });

    debug!("create_executable: copy executable {:?} to {:?}",
        exe, output);

    ensure_success(
        Command::new("cp")
           .arg(exe)
           .arg(output));
}
