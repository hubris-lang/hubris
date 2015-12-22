use llvm_sys;
use llvm_sys::prelude::*;

pub struct Context {
    ctxt_ref: LLVMContextRef,
}

impl Context {
    pub fn new() -> Context {
        Context {
            ctxt_ref: unsafe { llvm_sys::core::LLVMContextCreate() },
        }
    }

    #[inline]
    pub unsafe fn as_ptr(&self) -> LLVMContextRef {
        self.ctxt_ref
    }

    pub fn void_type(&self) -> LLVMTypeRef {
        unsafe {
            llvm_sys::core::LLVMVoidTypeInContext(self.as_ptr())
        }
    }

    pub fn i64_type(&self) -> LLVMTypeRef {
        unsafe {
            llvm_sys::core::LLVMInt64TypeInContext(self.as_ptr())
        }
    }
}

impl Drop for Context {
    fn drop(&mut self) {
        unsafe { llvm_sys::core::LLVMContextDispose(self.ctxt_ref) };
    }
}
