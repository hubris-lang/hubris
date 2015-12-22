use llvm_sys;
use llvm_sys::prelude::*;

use super::context::Context;

pub struct Function {
    pub name: String,
    pub ty: LLVMTypeRef,
    pub function_ref: LLVMValueRef,
    pub entry_block: Option<LLVMBasicBlockRef>,
}

#[derive(Clone)]
pub struct FunctionType {
    ret: LLVMTypeRef,
    args: Vec<LLVMTypeRef>,
}

impl FunctionType {
    pub fn new(ret: LLVMTypeRef, args: Vec<LLVMTypeRef>) -> FunctionType {
        FunctionType {
            ret: ret,
            args: args,
        }
    }

    pub fn to_value(mut self) -> LLVMTypeRef {
        unsafe {
            llvm_sys::core::LLVMFunctionType(
                self.ret,
                self.args.as_mut_ptr(),
                self.args.len() as u32, 0)
        }
    }
}

impl Function {
    pub fn in_module(module: &super::module::Module,
                     mut name: String,
                     mut fn_ty: FunctionType) -> Function { unsafe {
        let fn_ty = fn_ty.to_value();

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
            entry_block: None,
        }
    } }

    pub fn create_entry_block(&mut self, cx: &Context) -> LLVMBasicBlockRef {
        let bb = self.append_basic_block(cx, "entry".to_string());
        self.entry_block = Some(bb);
        return bb;
    }

    pub fn append_basic_block(&self, cx: &Context, mut label: String) -> LLVMBasicBlockRef {
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
