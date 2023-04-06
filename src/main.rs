extern crate combine;
mod compile;
mod expr;
mod lex;
mod parse;
mod statement;
mod token;
use compile::{compile_statement, init, print_ir};
use lex::lex;
use parse::parse;

use crate::compile::deinit;

fn main() {
    let input = "def f(x, y) (x * x) + (2.0 * x * y) + (y * y)";
    let lexed = lex(input);
    println!("{:?}", lexed);
    let unwrapped = lexed.unwrap().0;
    let parsed = parse(&unwrapped[..]);
    println!("{:?}", parsed);
    let mut ctx = init();
    dbg!("about to compile statement");
    let compiled = compile_statement(&mut ctx, &parsed.unwrap().0[0]);
    dbg!("done compiling!");
    print_ir(&compiled.unwrap());
    deinit(ctx);
}
