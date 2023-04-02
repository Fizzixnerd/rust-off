extern crate combine;
use combine::parser::char::{alpha_num, digit, letter, spaces, string};
use combine::{choice, many, many1, one_of, ParseError, Parser, Stream};

fn main() {}

fn def<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("def").map(|_| Token::Def)
}

fn extern_<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("extern").map(|_| Token::Extern)
}

fn lparen<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("(").map(|_| Token::LParen)
}

fn rparen<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string(")").map(|_| Token::RParen)
}

fn if_<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("if").map(|_| Token::If)
}

fn then<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("then").map(|_| Token::Then)
}

fn else_<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("else").map(|_| Token::Else)
}

fn for_<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("for").map(|_| Token::For)
}

fn do_<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string("do").map(|_| Token::Do)
}

fn colon<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string(":").map(|_| Token::Colon)
}

fn comma<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    string(",").map(|_| Token::Comma)
}

fn identifier<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    letter()
        .and(many1(alpha_num()))
        .map(|(c, rest): (char, String)| Token::Identifier(c.to_string() + &rest))
}

fn operator<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many1(one_of("+-*/^".chars())).map(Token::Operator)
}

fn floating<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many1(digit()).and(string(".")).and(many(digit())).map(
        |((before_digits, decimal_place), after_digits): ((String, &str), String)| {
            Token::Floating(
                (before_digits + &decimal_place + &after_digits)
                    .parse::<f64>()
                    .unwrap(),
            )
        },
    )
}

pub fn token<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice!(
        def(),
        extern_(),
        lparen(),
        rparen(),
        if_(),
        then(),
        else_(),
        for_(),
        do_(),
        colon(),
        comma(),
        operator(),
        identifier(),
        floating()
    )
}

pub fn tokens<Input>() -> impl Parser<Input, Output = Vec<Token>>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    spaces().with(many(token().skip(spaces())))
}

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
