extern crate llvm_sys;

use combine::error;
use llvm_sys::{
    analysis::LLVMVerifyFunction,
    core::{
        LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMBuildCall2, LLVMBuildFAdd,
        LLVMBuildFCmp, LLVMBuildFDiv, LLVMBuildFMul, LLVMBuildFSub, LLVMBuildRet, LLVMBuildUIToFP,
        LLVMConstReal, LLVMContextCreate, LLVMContextDispose, LLVMCountBasicBlocks,
        LLVMCountParams, LLVMCreateBuilderInContext, LLVMCreateFunctionPassManager,
        LLVMCreateFunctionPassManagerForModule, LLVMCreatePassManager, LLVMDeleteFunction,
        LLVMDisposeBuilder, LLVMDisposeModule, LLVMDisposePassManager, LLVMDoubleTypeInContext,
        LLVMFunctionType, LLVMGetGlobalPassRegistry, LLVMGetNamedFunction, LLVMGetParam,
        LLVMGetValueName2, LLVMGlobalGetValueType, LLVMModuleCreateWithNameInContext,
        LLVMPositionBuilderAtEnd, LLVMPrintModuleToString, LLVMPrintValueToString, LLVMSetLinkage,
        LLVMSetValueName2, LLVMShutdown, LLVMTypeOf,
    },
    error::LLVMGetErrorMessage,
    execution_engine::LLVMLinkInMCJIT,
    initialization::{LLVMInitializeCodeGen, LLVMInitializeCore},
    orc2::{
        lljit::{
            LLVMOrcCreateLLJIT, LLVMOrcCreateLLJITBuilder, LLVMOrcDisposeLLJIT,
            LLVMOrcLLJITAddLLVMIRModule, LLVMOrcLLJITRef,
        },
        LLVMOrcCreateNewThreadSafeContext, LLVMOrcThreadSafeContextRef,
    },
    prelude::{LLVMBuilderRef, LLVMContextRef, LLVMModuleRef, LLVMValueRef},
    target::{
        LLVM_InitializeAllAsmParsers, LLVM_InitializeAllAsmPrinters, LLVM_InitializeAllTargetInfos,
        LLVM_InitializeAllTargetMCs, LLVM_InitializeAllTargets,
    },
    target_machine::{
        LLVMCreateTargetMachine, LLVMDisposeTargetMachine, LLVMGetDefaultTargetTriple,
        LLVMGetHostCPUFeatures, LLVMGetHostCPUName, LLVMGetTargetDescription,
        LLVMGetTargetFromTriple, LLVMOpaqueTargetMachine, LLVMTarget, LLVMTargetMachineRef,
        LLVMTargetRef,
    },
    transforms::pass_builder::{
        LLVMCreatePassBuilderOptions, LLVMDisposePassBuilderOptions, LLVMPassBuilderOptionsRef,
    },
    LLVMBuilder, LLVMContext, LLVMModule, LLVMPassManager, LLVMValue,
};
use std::collections::hash_map::HashMap;
use std::ffi::CString;
use std::ptr::null_mut;

use crate::{
    expr::{Atom, BinaryOp, Expr},
    statement::{FunctionDecl, FunctionDefn, Program, Statement},
};

pub struct CompilationContext {
    pub llvm_context: LLVMContextRef,
    pub llvm_builder: LLVMBuilderRef,
    pub llvm_module: LLVMModuleRef,
    pub llvm_target_machine: LLVMTargetMachineRef,
    pub llvm_pass_builder_options: LLVMPassBuilderOptionsRef,
    pub llvm_jit: LLVMOrcLLJITRef,
    pub llvm_jit_context: LLVMOrcThreadSafeContextRef,
    pub opt_passes: Vec<String>,
    pub symbol_table: HashMap<String, LLVMValueRef>,
}

/*#[inline]
pub fn is_c_true(x: i32) -> bool {
    x != 0
} */

#[inline]
pub fn from_ffi_string(string: &*const i8, len: usize) -> String {
    let mut v: Vec<u8> = Vec::new();
    unsafe {
        for i in 0..len {
            let c: u8 = *string.add(i) as u8;
            v.push(c);
        }
    }
    let cstring = CString::new(v).unwrap();
    cstring.into_string().unwrap()
}

pub fn init() -> Option<CompilationContext> {
    let module_name = CString::new("rustoff_main").unwrap();
    let module_name_ptr = module_name.as_ptr();
    unsafe {
        let llvm_pass_registry_ref = LLVMGetGlobalPassRegistry();
        LLVMInitializeCore(llvm_pass_registry_ref);
        LLVMInitializeCodeGen(llvm_pass_registry_ref);
        LLVM_InitializeAllTargets();
        LLVM_InitializeAllTargetInfos();
        LLVM_InitializeAllTargetMCs();
        LLVM_InitializeAllAsmParsers();
        LLVM_InitializeAllAsmPrinters();
        LLVMLinkInMCJIT();
        let llvm_context = LLVMContextCreate();
        let llvm_jit_context = LLVMOrcCreateNewThreadSafeContext();
        let llvm_builder = LLVMCreateBuilderInContext(llvm_context);
        let llvm_module = LLVMModuleCreateWithNameInContext(module_name_ptr, llvm_context);
        // BEGIN INITIALIZE TARGET
        let default_target_triple = LLVMGetDefaultTargetTriple();
        let host_cpu_name = LLVMGetHostCPUName();
        let host_cpu_features = LLVMGetHostCPUFeatures();
        let mut target: LLVMTargetRef = null_mut();
        let target_ptr = &mut target as *mut _;
        let mut error_message: *mut i8 = null_mut();
        let error_message_ptr = &mut error_message as *mut _;
        LLVMGetTargetFromTriple(default_target_triple, target_ptr, error_message_ptr);
        // now `target` is our target.
        if error_message != null_mut() {
            let dtt: *const _ = default_target_triple;
            dbg!(from_ffi_string(&dtt, libc::strlen(dtt)));
            let msg: *const _ = error_message;
            dbg!(from_ffi_string(&msg, libc::strlen(msg)));
            return None;
        }
        let llvm_target_machine = LLVMCreateTargetMachine(
            target,
            default_target_triple,
            host_cpu_name,
            host_cpu_features,
            llvm_sys::target_machine::LLVMCodeGenOptLevel::LLVMCodeGenLevelDefault,
            llvm_sys::target_machine::LLVMRelocMode::LLVMRelocPIC,
            llvm_sys::target_machine::LLVMCodeModel::LLVMCodeModelJITDefault,
        );
        let llvm_pass_builder_options = LLVMCreatePassBuilderOptions();
        let opt_passes: Vec<String> = vec!["default<O2>".to_string()];
        // END INITIALIZE TARGET
        let mut llvm_jit: LLVMOrcLLJITRef = null_mut();
        let llvm_jit_ptr = &mut llvm_jit as *mut _;
        let lljit_builder = LLVMOrcCreateLLJITBuilder();
        let err = LLVMOrcCreateLLJIT(llvm_jit_ptr, lljit_builder);
        if llvm_jit == null_mut() {
            let err_msg = LLVMGetErrorMessage(err).cast_const();
            let err_msg_string = from_ffi_string(&err_msg, libc::strlen(err_msg));
            println!("ERROR: {}", err_msg_string);
            return None;
        }
        let symbol_table = HashMap::new();
        Some(CompilationContext {
            llvm_context,
            llvm_builder,
            llvm_module,
            llvm_target_machine,
            llvm_pass_builder_options,
            llvm_jit,
            llvm_jit_context,
            opt_passes,
            symbol_table,
        })
    }
}

pub fn deinit(ctx: CompilationContext) {
    unsafe {
        LLVMOrcDisposeLLJIT(ctx.llvm_jit);
        LLVMDisposePassBuilderOptions(ctx.llvm_pass_builder_options);
        LLVMDisposeTargetMachine(ctx.llvm_target_machine);
        LLVMDisposeModule(ctx.llvm_module);
        LLVMDisposeBuilder(ctx.llvm_builder);
        LLVMContextDispose(ctx.llvm_context);
        LLVMShutdown();
    }
}

fn compile_atomic_expr(ctx: &CompilationContext, atom: &Atom) -> Option<*mut LLVMValue> {
    unsafe {
        match atom {
            Atom::Float(val) => Some(LLVMConstReal(
                LLVMDoubleTypeInContext(ctx.llvm_context),
                *val,
            )),
            Atom::Identifier(name) => ctx.symbol_table.get(name).map(|x| *x),
        }
    }
}

fn compile_binary_expr(
    ctx: &CompilationContext,
    op: &BinaryOp,
    lhs: &Box<Expr>,
    rhs: &Box<Expr>,
) -> Option<*mut LLVMValue> {
    let name = CString::new(match op {
        BinaryOp::Add => "faddtmp",
        BinaryOp::Sub => "fsubtmp",
        BinaryOp::Mul => "fmultmp",
        BinaryOp::Div => "fdivtmp",
        BinaryOp::Lt => "flttmp",
        _ => "tmp",
    })
    .unwrap();
    let to_bool_name = CString::new("booltmp").unwrap();
    let lhs_value = compile_expr(ctx, lhs);
    let rhs_value = compile_expr(ctx, rhs);
    unsafe {
        match (lhs_value, rhs_value) {
            (Some(l), Some(r)) => match op {
                BinaryOp::Add => Some(LLVMBuildFAdd(ctx.llvm_builder, l, r, name.as_ptr())),
                BinaryOp::Sub => Some(LLVMBuildFSub(ctx.llvm_builder, l, r, name.as_ptr())),
                BinaryOp::Mul => Some(LLVMBuildFMul(ctx.llvm_builder, l, r, name.as_ptr())),
                BinaryOp::Div => Some(LLVMBuildFDiv(ctx.llvm_builder, l, r, name.as_ptr())),
                BinaryOp::Lt => Some(LLVMBuildUIToFP(
                    ctx.llvm_builder,
                    LLVMBuildFCmp(
                        ctx.llvm_builder,
                        llvm_sys::LLVMRealPredicate::LLVMRealULT,
                        l,
                        r,
                        name.as_ptr(),
                    ),
                    LLVMDoubleTypeInContext(ctx.llvm_context),
                    to_bool_name.as_ptr(),
                )),
                BinaryOp::Gt => Some(LLVMBuildUIToFP(
                    ctx.llvm_builder,
                    LLVMBuildFCmp(
                        ctx.llvm_builder,
                        llvm_sys::LLVMRealPredicate::LLVMRealUGT,
                        l,
                        r,
                        name.as_ptr(),
                    ),
                    LLVMDoubleTypeInContext(ctx.llvm_context),
                    to_bool_name.as_ptr(),
                )),
                BinaryOp::Eq => Some(LLVMBuildUIToFP(
                    ctx.llvm_builder,
                    LLVMBuildFCmp(
                        ctx.llvm_builder,
                        llvm_sys::LLVMRealPredicate::LLVMRealUEQ,
                        l,
                        r,
                        name.as_ptr(),
                    ),
                    LLVMDoubleTypeInContext(ctx.llvm_context),
                    to_bool_name.as_ptr(),
                )),
                BinaryOp::Neq => Some(LLVMBuildUIToFP(
                    ctx.llvm_builder,
                    LLVMBuildFCmp(
                        ctx.llvm_builder,
                        llvm_sys::LLVMRealPredicate::LLVMRealUNE,
                        l,
                        r,
                        name.as_ptr(),
                    ),
                    LLVMDoubleTypeInContext(ctx.llvm_context),
                    to_bool_name.as_ptr(),
                )),
            },
            _ => None,
        }
    }
}

fn compile_funcall_expr(
    ctx: &CompilationContext,
    id: &String,
    exprs: &Vec<Expr>,
) -> Option<*mut LLVMValue> {
    let func_name = CString::new(id.as_str()).unwrap();
    let calltmp = CString::new("calltmp").unwrap();
    unsafe {
        let callee = LLVMGetNamedFunction(ctx.llvm_module, func_name.as_ptr());
        if callee == null_mut() || LLVMCountParams(callee) != exprs.len().try_into().unwrap() {
            None
        } else {
            let args: Option<Vec<*mut LLVMValue>> = exprs
                .iter()
                .map(|e| compile_expr(ctx, e))
                .collect::<Option<Vec<_>>>();
            match args {
                None => None,
                Some(mut xs) => Some(LLVMBuildCall2(
                    ctx.llvm_builder,
                    LLVMGlobalGetValueType(callee),
                    callee,
                    xs.as_mut_ptr(),
                    xs.len().try_into().unwrap(),
                    calltmp.as_ptr(),
                )),
            }
        }
    }
}

pub fn compile_expr(ctx: &CompilationContext, expr: &Expr) -> Option<*mut LLVMValue> {
    match expr {
        Expr::AtomicExpr(atom) => dbg!(compile_atomic_expr(ctx, atom)),
        Expr::BinaryExpr(op, lhs, rhs) => dbg!(compile_binary_expr(ctx, op, lhs, rhs)),
        Expr::FuncallExpr(id, es) => dbg!(compile_funcall_expr(ctx, id, es)),
        Expr::ParensExpr(expr) => dbg!(compile_expr(ctx, expr.as_ref())),
        _ => None,
    }
}

fn compile_function_decl_statement(
    ctx: &CompilationContext,
    decl: &FunctionDecl,
) -> Option<*mut LLVMValue> {
    let mut params = Vec::new();
    let func_name = CString::new(decl.name.as_str()).unwrap();
    unsafe {
        for _ in 0..decl.type_ {
            let param_type = LLVMDoubleTypeInContext(ctx.llvm_context);
            params.push(param_type);
        }
        let func_type = LLVMFunctionType(
            LLVMDoubleTypeInContext(ctx.llvm_context),
            params.as_mut_ptr(),
            decl.type_.try_into().unwrap(),
            false.try_into().unwrap(),
        );
        let function = LLVMAddFunction(ctx.llvm_module, func_name.as_ptr(), func_type);
        LLVMSetLinkage(function, llvm_sys::LLVMLinkage::LLVMExternalLinkage);
        for i in 0..decl.type_ {
            let param_name = CString::new(decl.args[i].as_str()).unwrap();
            let param = LLVMGetParam(function, i.try_into().unwrap());
            LLVMSetValueName2(param, param_name.as_ptr(), decl.args[i].len());
        }
        Some(function)
    }
}

fn compile_function_defn_statement(
    ctx: &mut CompilationContext,
    defn: &FunctionDefn,
) -> Option<*mut LLVMValue> {
    dbg!("compiling defn");
    let func_name = CString::new(defn.declaration.name.as_str()).unwrap();
    unsafe {
        let maybe_function = LLVMGetNamedFunction(ctx.llvm_module, func_name.as_ptr());
        dbg!("compiling decl (for defn)");
        let function = if maybe_function == null_mut() {
            dbg!(compile_function_decl_statement(ctx, &defn.declaration))
        } else {
            dbg!(Some(maybe_function))
        };
        match function {
            None => None,
            Some(func) => {
                if LLVMCountBasicBlocks(func) == 0 {
                    let block_name = CString::new("entry").unwrap();
                    let entry_block = dbg!(LLVMAppendBasicBlockInContext(
                        ctx.llvm_context,
                        func,
                        block_name.as_ptr()
                    ));
                    LLVMPositionBuilderAtEnd(ctx.llvm_builder, entry_block);
                    ctx.symbol_table.clear();
                    let n = LLVMCountParams(func);
                    for i in 0..n {
                        let param = LLVMGetParam(func, i);
                        let mut name_len_val: usize = 0;
                        let name_len_ptr = &mut name_len_val as *mut _;
                        let param_name_ptr = dbg!(LLVMGetValueName2(param, name_len_ptr));
                        let param_name_string = from_ffi_string(&param_name_ptr, name_len_val);
                        ctx.symbol_table.insert(param_name_string, param);
                    }
                    dbg!(func);
                    if let Some(value) = compile_expr(ctx, &defn.body) {
                        LLVMBuildRet(ctx.llvm_builder, value);
                        LLVMVerifyFunction(
                            func,
                            llvm_sys::analysis::LLVMVerifierFailureAction::LLVMPrintMessageAction,
                        );
                    }
                    Some(func)
                } else {
                    LLVMDeleteFunction(func);
                    None
                }
            }
        }
    }
}

fn compile_expr_statement(
    ctx: &mut CompilationContext,
    expr: &Expr,
    line_number: &Option<usize>,
) -> Option<*mut LLVMValue> {
    compile_function_defn_statement(
        ctx,
        &FunctionDefn {
            declaration: anonymous_decl(line_number),
            body: expr.clone(),
        },
    )
}

fn anonymous_decl(line_number: &Option<usize>) -> FunctionDecl {
    /* let name = match line_number {
        None => "__anonymous_func".to_string(),
        Some(ln) => format!("__anonymous_func_{}", ln).to_string(),
    }; */
    let name = "main".to_string();
    FunctionDecl {
        name,
        type_: 0,
        args: Vec::new(),
    }
}

pub fn compile_statement(
    ctx: &mut CompilationContext,
    stmt: &Statement,
    line_number: &Option<usize>,
) -> Option<*mut LLVMValue> {
    match stmt {
        Statement::FunctionDeclStatement(decl) => dbg!(compile_function_decl_statement(ctx, decl)),
        Statement::FunctionDefnStatement(defn) => dbg!(compile_function_defn_statement(ctx, defn)),
        Statement::ExprStatement(expr) => dbg!(compile_expr_statement(ctx, expr, line_number)),
    }
}

pub fn print_ir(module: &*mut LLVMModule) {
    unsafe {
        let value_c_str = LLVMPrintModuleToString(*module).cast_const();
        let value_string = from_ffi_string(&value_c_str, libc::strlen(value_c_str));
        println!("{}", value_string);
    }
}

pub fn compile(
    ctx: &mut CompilationContext,
    prog: &Program,
    line_number: &Option<usize>,
) -> Option<Vec<*mut LLVMValue>> {
    let mut v = Vec::new();
    for stmt in prog {
        let compiled = compile_statement(ctx, stmt, line_number);
        v.push(compiled);
    }
    v.into_iter().collect()
}
