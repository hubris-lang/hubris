use llvm_sys;
use llvm_sys::prelude::*;

use super::context::Context;

pub struct Function {
    pub name: String,
    pub ty: LLVMTypeRef,
    pub function_ref: LLVMValueRef,
}

#[derive(Clone)]
pub struct FunctionType {
    ret: LLVMTypeRef,
    args: Vec<LLVMTypeRef>
}

impl FunctionType {
    pub fn new(ret: LLVMTypeRef, args: Vec<LLVMTypeRef>) -> FunctionType {
        FunctionType {
            ret: ret,
            args: args,
        }
    }
}

impl Function {
    pub fn in_module(module: &super::module::Module,
                     mut name: String,
                     mut fn_ty: FunctionType) -> Function { unsafe {
        let fn_ty = llvm_sys::core::LLVMFunctionType(
            fn_ty.ret,
            fn_ty.args.as_mut_ptr(),
            fn_ty.args.len() as u32, 0);

        name.push('\0');

        let function = unsafe {
            llvm_sys::core::LLVMAddFunction(
                    module.as_ptr(),
                    name.as_ptr() as *const _,
                    fn_ty)
        };

        Function {
            name: name,
            ty: fn_ty,
            function_ref: function,
        }
    } }

    pub fn append_bb_in_ctxt(&self, cx: &Context, mut label: String) -> LLVMBasicBlockRef {
        unsafe {
            label.push('\0');
            llvm_sys::core::LLVMAppendBasicBlockInContext(
               cx.as_ptr(), self.as_ptr(),
               label.as_ptr() as *const _)
        }
    }

    pub unsafe fn as_ptr(&self) -> LLVMValueRef {
        self.function_ref
    }

    pub fn set_linkage(&self, linkage: llvm_sys::LLVMLinkage) {
        unsafe {
            llvm_sys::core::LLVMSetLinkage(self.function_ref, linkage);
        }
    }
}
