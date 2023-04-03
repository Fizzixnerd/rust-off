#[derive(Debug, Clone, PartialEq)]
pub enum Token {
    Def,
    Extern,
    LParen,
    RParen,
    If,
    Then,
    Else,
    For,
    Do,
    Colon,
    Comma,
    Operator(String),
    Identifier(String),
    Floating(f64),
}

pub type TokenStream = Vec<Token>;
