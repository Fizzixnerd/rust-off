extern crate combine;
mod expr;
mod lex;
mod parse;
mod statement;
mod token;
use lex::lex;
use parse::parse;

fn main() {
    let input = "if x < 3. then 4. else 3.1";
    let lexed = lex(input);
    println!("{:?}", lexed);
    let unwrapped = lexed.unwrap().0;
    let parsed = parse(&unwrapped[..]);
    println!("{:?}", parsed);
}
