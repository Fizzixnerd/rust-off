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
    TypeIdentifier(String),
    Floating(f64),
    USize(usize),
}

pub type TokenStream = Vec<Token>;
