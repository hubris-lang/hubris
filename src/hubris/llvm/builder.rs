use llvm_sys;
use llvm_sys::prelude::*;
use super::context::Context;

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
}

impl Drop for Builder {
    fn drop(&mut self) {
        unsafe {
            llvm_sys::core::LLVMDisposeBuilder(self.builder_ref);
        }
    }
}
