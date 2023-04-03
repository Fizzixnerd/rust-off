use crate::token::{Token, TokenStream};
use combine::easy::Errors;
use combine::parser::char::{alpha_num, digit, letter, space, spaces, string};
use combine::{
    attempt, choice, many, many1, one_of, EasyParser, ParseError, Parser, Stream, StreamOnce,
};

fn def<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt(string("def").map(|_| Token::Def).skip(space()))
}

fn extern_<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt(string("extern").map(|_| Token::Extern).skip(space()))
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
    attempt(string("if").map(|_| Token::If).skip(space()))
}

fn then<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt(string("then").map(|_| Token::Then).skip(space()))
}

fn else_<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt(string("else").map(|_| Token::Else).skip(space()))
}

fn for_<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt(string("for").map(|_| Token::For).skip(space()))
}

fn do_<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt(string("do").map(|_| Token::Do).skip(space()))
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
        .and(many(alpha_num()))
        .map(|(c, rest): (char, String)| Token::Identifier(c.to_string() + &rest))
}

fn operator<Input>() -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = char>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many1(one_of("!<>+-*/^=".chars())).map(Token::Operator)
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

pub fn lex<Input>(
    input: Input,
) -> Result<
    (TokenStream, Input),
    Errors<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
>
where
    Input: Stream<Token = char>,
    Input::Position: Default,
    Input::Range: PartialEq,
{
    tokens().easy_parse(input)
}
