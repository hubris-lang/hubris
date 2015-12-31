use llvm_sys;
use llvm_sys::prelude::*;

use super::context::Context;
use super::function::{Function};

pub struct Builder {
    builder_ref: LLVMBuilderRef
}

impl Builder {
    pub fn in_context(cx: &Context) -> Builder {
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
                args.as_mut_ptr(), args.len() as u32,
                b"bleh\0".as_ptr() as *const _)
        }
    }

    pub fn emit_ret_void(&self) -> LLVMValueRef {
        unsafe { llvm_sys::core::LLVMBuildRetVoid(self.as_ptr()) }
    }

    pub fn emit_ret(&self, value: LLVMValueRef) -> LLVMValueRef {
        unsafe { llvm_sys::core::LLVMBuildRet(self.as_ptr(), value) }
    }

    pub fn emit_alloca(&self, ty: LLVMTypeRef, s: String) -> LLVMValueRef {
        unsafe {
            llvm_sys::core::LLVMBuildAlloca(
                self.as_ptr(),
                ty,
                s.as_ptr() as *const _)
        }
    }

    pub fn postition_at_end(&self, bb: LLVMBasicBlockRef) {
        unsafe { llvm_sys::core::LLVMPositionBuilderAtEnd(self.as_ptr(), bb) }
    }
}

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe {
            llvm_sys::core::LLVMDisposeBuilder(self.builder_ref);
        }
    }
}
