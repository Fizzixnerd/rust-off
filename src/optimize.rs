use llvm_sys::transforms::pass_builder::LLVMRunPasses;
use std::ffi::CString;

use crate::compile::CompilationContext;

pub fn optimize(ctx: &CompilationContext) {
    let cstring = CString::new(ctx.opt_passes[0].as_str()).unwrap();
    unsafe {
        LLVMRunPasses(
            ctx.llvm_module,
            cstring.as_ptr(),
            ctx.llvm_target_machine,
            ctx.llvm_pass_builder_options,
        );
    }
}
