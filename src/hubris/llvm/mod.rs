use std::ptr;
use std::path::Path;

use llvm_sys;

pub mod builder;
pub mod context;
pub mod function;
pub mod module;
pub mod tools;

pub use self::builder::Builder;
pub use self::context::Context;
pub use self::module::Module;
pub use self::function::Function;

pub fn generate_ir<T: AsRef<Path>>(name: String, output: T) {
    let mut context = Context::new();
    let module = Module::with_name(name);
    module.set_target("x86_64-apple-darwin".to_string());
    let builder = Builder::in_context(&mut context);

    unsafe {
        // Set up a context, module and builder in that context.

        // Get the type signature for void nop(void);
        // Then create it in our module.
        let void = llvm_sys::core::LLVMVoidTypeInContext(context.as_ptr());
        let function_type = llvm_sys::core::LLVMFunctionType(void, ptr::null_mut(), 0, 0);

        let function = module.add_function("_hubris_main".to_string(), function_type);
        let efunction = module.add_function("hello_world".to_string(), function_type);

        efunction.set_linkage(llvm_sys::LLVMLinkage::LLVMExternalLinkage);

        // Create a basic block in the function and set our builder to generate
        // code in it.
        let bb = llvm_sys::core::LLVMAppendBasicBlockInContext(
            context.as_ptr(), function.as_ptr(),
            b"entry\0".as_ptr() as *const _);

        llvm_sys::core::LLVMPositionBuilderAtEnd(builder.as_ptr(), bb);

        builder.emit_call(&efunction, vec![]);

        // Emit a `ret void` into the function
        llvm_sys::core::LLVMBuildRetVoid(builder.as_ptr());
    }

    module.dump();
    module.print_to_file(output);
}
