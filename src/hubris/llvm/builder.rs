use llvm_sys;
use llvm_sys::prelude::*;

use super::context::Context;
use super::function::Function;

pub struct Builder {
    builder_ref: LLVMBuilderRef
}

impl Builder {
    pub fn in_context(cx: &mut Context) -> Builder {
        Builder {
            builder_ref: unsafe {
                llvm_sys::core::LLVMCreateBuilderInContext(cx.as_ptr())
            }
        }
    }

    #[inline]
    pub unsafe fn as_ptr(&self) -> LLVMBuilderRef {
        self.builder_ref
    }

    pub fn emit_call(&self, fun: &Function, mut args: Vec<LLVMValueRef>) -> LLVMValueRef {
        unsafe {
            llvm_sys::core::LLVMBuildCall(
                self.as_ptr(), fun.as_ptr(),
                [].as_mut_ptr(), args.len() as u32,
                b"\0".as_ptr() as *const _)
        }
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe {
            llvm_sys::core::LLVMDisposeBuilder(self.builder_ref);
        }
    }
}
