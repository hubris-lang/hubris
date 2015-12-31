use std::path::Path;
use std::mem::transmute;

use super::function::Function;

use llvm_sys;
use llvm_sys::prelude::*;

pub struct Module {
    module_ref: LLVMModuleRef
}

impl Module {
    pub fn with_name(mut s: String) -> Module {
        s.push('\0');
        Module {
            module_ref: unsafe {
                llvm_sys::core::LLVMModuleCreateWithName(s.as_ptr() as *const _)
            }
        }
    }

    #[inline]
    pub unsafe fn as_ptr(&self) -> LLVMModuleRef {
        self.module_ref
    }

    pub fn dump(&self) {
        unsafe {
            llvm_sys::core::LLVMDumpModule(self.as_ptr());
        }
    }

    pub fn print_to_file<P: AsRef<Path>>(&self, path: P) {
        let output = format!("{}", path.as_ref().display());

        unsafe {
            llvm_sys::core::LLVMPrintModuleToFile(
                self.as_ptr(),
                output.as_ptr() as *const _,
                b"yolo.ll\0".as_ptr() as *mut _);
        }
    }

    pub fn get_function(&self, mut name: String) -> Option<Function> {
        unsafe {
            name.push('\0');

            let fun_ref = llvm_sys::core::LLVMGetNamedFunction(
                self.as_ptr(),
                name.as_ptr() as *const _);

            // let fn_ty = llvm_sys::core::LLVMGetTypeByName(
            //     self.as_ptr(),
            //     name.as_ptr() as *const _);

            Some(Function {
                name: name,
                ty: transmute(0 as i64),
                function_ref: fun_ref,
                entry_block: None,
            })
        }
    }

    pub fn set_target(&self, mut triple: String) {
        triple.push('\0');
        unsafe {
            llvm_sys::core::LLVMSetTarget(self.as_ptr(), triple.as_ptr() as *const _)
        }
    }
}

impl Drop for Module {
    fn drop(&mut self) {
        unsafe { llvm_sys::core::LLVMDisposeModule(self.module_ref) }
    }
}
