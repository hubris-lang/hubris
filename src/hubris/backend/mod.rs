use std::collections::HashMap;
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

#[derive(Debug, Clone)]
enum Error {
    UnknownSymbol(String),
}

struct ModuleCx<'cx, 'm> {
    cx: &'cx llvm::Context,
    module: llvm::Module,
    cps_module: &'m cps::Module,
}

impl<'cx, 'm> ModuleCx<'cx, 'm> {
    fn new(cx: &'cx llvm::Context, m: &'m cps::Module) -> ModuleCx<'cx, 'm> {
        let module = llvm::Module::with_name(m.name.to_string());
        module.set_target("x86_64-apple-darwin".to_string());

        ModuleCx {
            cx: cx,
            module: module,
            cps_module: m,
        }
    }

    pub fn emit_module<P: AsRef<Path> + Debug>(&self, output: P) -> Result<(), Error> {
        for def in &self.cps_module.defs {
            match def {
                &cps::Definition::Fn(ref fun) => {
                    let mut fcx = FunctionCx::new(self, fun);
                    try!(fcx.emit_function());
                }
                &cps::Definition::Extern(ref e) => self.emit_extern(&e.0, &e.1),
                _ => {}
            }
        }

        self.module.dump();
        self.module.print_to_file(output);

        Ok(())
    }

    pub fn emit_extern(&self, name: &core::Name, ty: &core::Term) {
        let i64_ty = self.cx.i64_type();
        let fn_ty = llvm::FunctionType::new(i64_ty, vec![i64_ty]);

        let efunction = llvm::Function::in_module(
            &self.module,
            name.clone(),
            fn_ty);

        efunction.set_linkage(llvm_sys::LLVMLinkage::LLVMExternalLinkage);
    }
}

struct FunctionCx<'cx, 'm:'cx, 'f> {
    mcx: &'cx ModuleCx<'cx, 'm>,
    func: &'f cps::Function,
    builder: llvm::Builder,
    vars: HashMap<String, LLVMValueRef>,
}

impl<'cx, 'm, 'f> FunctionCx<'cx, 'm, 'f> {
    fn new(module: &'cx ModuleCx<'cx, 'm>, func: &'f cps::Function) -> FunctionCx<'cx, 'm, 'f> {
        FunctionCx {
            mcx: module,
            func: func,
            builder: llvm::Builder::in_context(module.cx),
            vars: HashMap::new(),
        }
    }

    pub fn emit_function(&mut self) -> Result<(), Error> {
        let arg_tys = self.func.args.iter().map(|_| {
            self.mcx.cx.i64_type()
        }).collect();

        let fn_ty = llvm::FunctionType::new(self.mcx.cx.i64_type(), arg_tys);

        let fname = if self.func.name == "main" {
            "_hubris_main".to_string()
        } else {
            self.func.name.clone()
        };

        let mut function = llvm::Function::in_module(
            &self.mcx.module,
            fname,
            fn_ty.clone());

        let bb = function.create_entry_block(&self.mcx.cx);
        self.builder.postition_at_end(bb);

        debug!("emit_fucntion: emitting body");
        let return_value = try!(self.emit_term(&self.func.body));

        debug!("emit_function: emitting return");
        self.builder.emit_ret(return_value);

        Ok(())
    }

    pub fn emit_term(&mut self, term: &cps::Term) -> Result<LLVMValueRef, Error> {
        use cps::Term::*;

        debug!("emit_term: term={:?}", term);

        match term {
            &Value(ref v) => self.emit_value(v),
            &App(ref callee, ref arg) => {
                debug!("emitting callee");
                let callee = try!(self.emit_value(callee));
                let arg = try!(self.emit_value(arg));

                Ok(unsafe {
                    llvm_sys::core::LLVMBuildCall(
                        self.builder.as_ptr(), callee,
                        [arg].as_mut_ptr(), 1 as u32,
                        b"call_tmp\0".as_ptr() as *const _)
                })
            }
            &Fix(ref bindings, ref body) => {
                debug!("emitting args");
                for &(ref n, ref args, ref term) in bindings {
                    if args.len() == 0 {
                        let i64_ty = self.mcx.cx.i64_type();
                        debug!("before alloc");
                        let alloca = self.builder.emit_alloca(i64_ty, n.clone());
                        debug!("before value");
                        let value = try!(self.emit_term(term));
                        // let value = unsafe { llvm_sys::core::LLVMBuildPtrToInt(
                        //     self.builder.as_ptr(),
                        //     value, i64_ty, b"a\0".as_ptr() as *const _) };
                        debug!("before store");
                        debug!("{:?} {:?}", value, alloca);
                        let sto = unsafe {
                            llvm_sys::core::LLVMBuildStore(
                                self.builder.as_ptr(),
                                value,
                                alloca)
                        };
                        self.vars.insert(n.clone(), alloca);
                    } else {
                        panic!("can't code gen local closures")
                    }
                }

                debug!("emitting body");
                self.emit_term(body)
            }
        }
    }

    pub fn emit_value(&self, value: &cps::Value) -> Result<LLVMValueRef, Error> {
        use cps::Value::*;

        debug!("emit_value: value={:?}", value);

        match value {
            &Literal(ref l) => self.emit_literal(l),
            &Var(ref name) => match self.vars.get(name) {
                None => match self.mcx.module.get_function(name.clone()) {
                    Some(function) => Ok(unsafe { function.as_ptr() }),
                    None => Err(Error::UnknownSymbol(name.clone())),
                },
                Some(v) => {
                    Ok(unsafe {
                        llvm_sys::core::LLVMBuildLoad(
                            self.builder.as_ptr(),
                            *v,
                            name.as_ptr() as *const _)
                    })
                }
            }
        }
    }

    pub fn emit_literal(&self, lit: &core::Literal) -> Result<LLVMValueRef, Error> {
        use core::Literal;

        match lit {
            &Literal::Unit => unsafe {
                let i64_ty = self.mcx.cx.i64_type();
                Ok(llvm_sys::core::LLVMConstInt(i64_ty, 1, 0))
            },
            lit => panic!("can't compile {:?}", lit),
        }
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
