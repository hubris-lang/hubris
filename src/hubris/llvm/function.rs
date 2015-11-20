use llvm_sys;
use llvm_sys::prelude::*;

pub struct Function {
    fn_ref: LLVMValueRef,
}

impl Function {
    pub unsafe fn from_ptr(ptr: LLVMValueRef) -> Function {
        Function { fn_ref: ptr }
    }

    pub unsafe fn as_ptr(&self) -> LLVMValueRef {
        self.fn_ref
    }

    pub fn set_linkage(&self, linkage: llvm_sys::LLVMLinkage) {
        unsafe {
            llvm_sys::core::LLVMSetLinkage(self.fn_ref, linkage);
        }
    }
}
