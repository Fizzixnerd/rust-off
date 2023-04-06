extern crate llvm_sys;

use llvm_sys::{
    analysis::LLVMVerifyFunction,
    core::{
        LLVMAddFunction, LLVMAppendBasicBlockInContext, LLVMBuildCall2, LLVMBuildFAdd,
        LLVMBuildFCmp, LLVMBuildFDiv, LLVMBuildFMul, LLVMBuildFSub, LLVMBuildRet, LLVMBuildUIToFP,
        LLVMConstReal, LLVMContextCreate, LLVMContextDispose, LLVMCountBasicBlocks,
        LLVMCountParams, LLVMCreateBuilderInContext, LLVMDeleteFunction, LLVMDisposeBuilder,
        LLVMDisposeModule, LLVMDoubleTypeInContext, LLVMFunctionType, LLVMGetGlobalPassRegistry,
        LLVMGetNamedFunction, LLVMGetParam, LLVMGetValueName2, LLVMModuleCreateWithNameInContext,
        LLVMPositionBuilderAtEnd, LLVMPrintValueToString, LLVMSetLinkage, LLVMSetValueName2,
        LLVMShutdown,
    },
    initialization::{LLVMInitializeCodeGen, LLVMInitializeCore},
    LLVMBuilder, LLVMContext, LLVMModule, LLVMValue,
};
use std::collections::hash_map::HashMap;
use std::ffi::CString;
use std::ptr::null_mut;

use crate::{
    expr::{Atom, BinaryOp, Expr},
    statement::{FunctionDecl, FunctionDefn, Statement},
};

pub struct CompilationContext {
    pub llvm_context: *mut LLVMContext,
    pub llvm_builder: *mut LLVMBuilder,
    pub llvm_module: *mut LLVMModule,
    pub symbol_table: HashMap<String, *mut LLVMValue>,
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

pub fn init() -> CompilationContext {
    let module_name = CString::new("rustoff_main").unwrap();
    let module_name_ptr = module_name.as_ptr();
    unsafe {
        let llvm_pass_registry_ref = LLVMGetGlobalPassRegistry();
        LLVMInitializeCore(llvm_pass_registry_ref);
        LLVMInitializeCodeGen(llvm_pass_registry_ref);
        let llvm_context = LLVMContextCreate();
        let llvm_builder = LLVMCreateBuilderInContext(llvm_context);
        let llvm_module = LLVMModuleCreateWithNameInContext(module_name_ptr, llvm_context);
        let symbol_table = HashMap::new();
        CompilationContext {
            llvm_context,
            llvm_builder,
            llvm_module,
            symbol_table,
        }
    }
}

pub fn deinit(ctx: CompilationContext) {
    unsafe {
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
                    LLVMDoubleTypeInContext(ctx.llvm_context),
                    callee,
                    xs.as_mut_ptr(),
                    exprs.len().try_into().unwrap(),
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

fn compile_expr_statement(ctx: &mut CompilationContext, expr: &Expr) -> Option<*mut LLVMValue> {
    compile_function_defn_statement(
        ctx,
        &FunctionDefn {
            declaration: anonymous_decl(),
            body: expr.clone(),
        },
    )
}

fn anonymous_decl() -> FunctionDecl {
    FunctionDecl {
        name: "__anonymous_func".to_string(),
        type_: 0,
        args: Vec::new(),
    }
}

pub fn compile_statement(ctx: &mut CompilationContext, stmt: &Statement) -> Option<*mut LLVMValue> {
    match stmt {
        Statement::FunctionDeclStatement(decl) => dbg!(compile_function_decl_statement(ctx, decl)),
        Statement::FunctionDefnStatement(defn) => dbg!(compile_function_defn_statement(ctx, defn)),
        Statement::ExprStatement(expr) => dbg!(compile_expr_statement(ctx, expr)),
    }
}

pub fn print_ir(func: &*mut LLVMValue) {
    unsafe {
        let val_c_str = LLVMPrintValueToString(*func).cast_const();
        let value_str = from_ffi_string(&val_c_str, libc::strlen(val_c_str));
        println!("{}", value_str);
    }
}
