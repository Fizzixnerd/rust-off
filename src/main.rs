extern crate combine;
mod compile;
mod expr;
mod lex;
mod optimize;
mod parse;
mod statement;
mod token;
use compile::{compile, deinit, init, print_ir, CompilationContext};
use lex::lex;
use llvm_sys::orc2::{
    lljit::{LLVMOrcLLJITAddLLVMIRModule, LLVMOrcLLJITGetMainJITDylib, LLVMOrcLLJITLookup},
    LLVMOrcCreateNewThreadSafeModule, LLVMOrcExecutorAddress,
};
use parse::parse;
use std::io;

struct ReplState {
    pub lines: Vec<String>,
    pub context: CompilationContext,
}

fn repl() {
    if let Some(ctx) = init() {
        let lines = Vec::new();
        let mut repl_state = ReplState {
            context: ctx,
            lines,
        };
        loop {
            let mut s = String::new();
            if let Ok(_) = io::stdin().read_line(&mut s) {
                if s.as_str() == "" {
                    break;
                } else {
                    repl_state.lines.push(s.clone());
                    let parsed = parse(&lex(s.as_str()).expect("Couldn't lex").0[..])
                        .expect("Couldn't parse")
                        .0;
                    compile(
                        &mut repl_state.context,
                        &parsed,
                        &Some(repl_state.lines.len()),
                    )
                    .expect("Couldn't compile");
                    // optimize(&repl_state.context);
                    print_ir(&repl_state.context.llvm_module);
                    unsafe {
                        let dylib = LLVMOrcLLJITGetMainJITDylib(repl_state.context.llvm_jit);
                        let tsm = LLVMOrcCreateNewThreadSafeModule(
                            repl_state.context.llvm_module,
                            repl_state.context.llvm_jit_context,
                        );
                        LLVMOrcLLJITAddLLVMIRModule(repl_state.context.llvm_jit, dylib, tsm);
                        let mut executor_addr: LLVMOrcExecutorAddress = 0;
                        let executor_addr_ptr = &mut executor_addr as *mut _;
                        let main_name = std::ffi::CString::new("main2").unwrap();
                        LLVMOrcLLJITLookup(
                            repl_state.context.llvm_jit,
                            executor_addr_ptr,
                            main_name.as_ptr(),
                        );
                        let func: fn() -> f64 = std::mem::transmute(executor_addr);
                        println!("{}", func());
                    };
                }
            } else {
                println!("Couldn't read line");
                break;
            }
        }
        deinit(repl_state.context);
    } else {
        println!("Can't start!");
    }
}

fn main() {
    repl();
}

fn main_test() {
    let input = "extern sin(x) extern cos(x) (sin(1.0) * sin(1.0)) + (cos(1.0) * cos(1.0))";
    let lexed = lex(input);
    println!("{:?}", lexed);
    let unwrapped = lexed.unwrap().0;
    let parsed = parse(&unwrapped[..]);
    println!("{:?}", parsed);
    let mut ctx = init().unwrap();
    compile(&mut ctx, &parsed.unwrap().0, &None);
    print_ir(&ctx.llvm_module);
    deinit(ctx);
}
