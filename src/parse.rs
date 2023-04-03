use combine::easy::Errors;
use combine::{
    attempt, choice, many1, parser, satisfy, satisfy_map, sep_by, EasyParser, ParseError, Parser,
    Stream, StreamOnce,
};

use crate::expr::{Atom, BinaryOp, Expr};
use crate::statement::{FunctionDecl, FunctionDefn, Program, Statement};
use crate::token::Token;

fn match_operator(t: Token) -> Option<BinaryOp> {
    match t {
        Token::Operator(binop) => match binop.as_str() {
            "+" => Some(BinaryOp::Add),
            "-" => Some(BinaryOp::Sub),
            "*" => Some(BinaryOp::Mul),
            "/" => Some(BinaryOp::Div),
            "<" => Some(BinaryOp::Lt),
            ">" => Some(BinaryOp::Gt),
            "==" => Some(BinaryOp::Eq),
            "!=" => Some(BinaryOp::Neq),
            _ => None,
        },
        _ => None,
    }
}

fn operator<Input>() -> impl Parser<Input, Output = BinaryOp>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    satisfy_map(match_operator)
}

fn match_identifier(t: Token) -> Option<String> {
    match t {
        Token::Identifier(i) => Some(i),
        _ => None,
    }
}

fn identifier<Input>() -> impl Parser<Input, Output = String>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    satisfy_map(match_identifier)
}

fn binary_expr<Input>() -> impl Parser<Input, Output = Expr>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt(expr_nonlrec().and(operator()))
        .and(expr())
        .map(|((e1, op), e2)| Expr::BinaryExpr(op, Box::new(e1), Box::new(e2)))
}

fn match_atom(t: Token) -> Option<Atom> {
    match t {
        Token::Floating(f) => Some(Atom::Float(f)),
        Token::Identifier(s) => Some(Atom::Identifier(s)),
        _ => None,
    }
}

fn atomic_expr<Input>() -> impl Parser<Input, Output = Expr>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    satisfy_map(match_atom).map(Expr::AtomicExpr)
}

fn eq_token<Input>(t: Token) -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    satisfy(move |s| s == t)
}

fn parens_expr<Input>() -> impl Parser<Input, Output = Expr>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    eq_token(Token::LParen)
        .with(expr())
        .skip(eq_token(Token::RParen))
        .map(|e| Expr::ParensExpr(Box::new(e)))
}

fn funcall_expr<Input>() -> impl Parser<Input, Output = Expr>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt(identifier().skip(eq_token(Token::LParen)))
        .and(sep_by(expr(), eq_token(Token::Comma)))
        .skip(eq_token(Token::RParen))
        .map(|(i, es)| Expr::FuncallExpr(i, es))
}

fn if_then_else_expr<Input>() -> impl Parser<Input, Output = Expr>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    eq_token(Token::If)
        .with(expr())
        .skip(eq_token(Token::Then))
        .and(expr())
        .skip(eq_token(Token::Else))
        .and(expr())
        .map(|((p, c), a)| Expr::IfThenElseExpr(Box::new(p), Box::new(c), Box::new(a)))
}

parser! {
    fn expr_nonlrec[Input]()(Input) -> Expr
    where [Input: Stream<Token = Token>]
    {
        choice!(atomic_expr(), if_then_else_expr(), parens_expr(), funcall_expr())
    }
}

parser! {
    fn expr[Input]()(Input) -> Expr
    where [Input: Stream<Token = Token>]
    {
       choice!(binary_expr(), expr_nonlrec())
    }
}

fn expr_statement<Input>() -> impl Parser<Input, Output = Statement>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    expr().map(Statement::ExprStatement)
}

fn function_decl<Input>() -> impl Parser<Input, Output = FunctionDecl>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt(identifier().skip(eq_token(Token::LParen)))
        .and(sep_by::<Vec<String>, _, _, _>(
            identifier(),
            eq_token(Token::Comma),
        ))
        .skip(eq_token(Token::RParen))
        .map(|(name, args)| FunctionDecl {
            name,
            type_: args.len(),
            args,
        })
}

fn function_decl_statement<Input>() -> impl Parser<Input, Output = Statement>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    eq_token(Token::Extern)
        .with(function_decl())
        .map(Statement::FunctionDeclStatement)
}

fn function_defn_statement<Input>() -> impl Parser<Input, Output = Statement>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    eq_token(Token::Def)
        .with(function_decl())
        .and(expr())
        .map(|(d, e)| {
            Statement::FunctionDefnStatement(FunctionDefn {
                declaration: d,
                body: e,
            })
        })
}

pub fn statement<Input>() -> impl Parser<Input, Output = Statement>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    choice!(
        expr_statement(),
        function_decl_statement(),
        function_defn_statement()
    )
}

pub fn program<Input>() -> impl Parser<Input, Output = Program>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many1(statement())
}

pub fn parse<Input>(
    input: Input,
) -> Result<
    (Program, Input),
    Errors<
        <Input as StreamOnce>::Token,
        <Input as StreamOnce>::Range,
        <Input as StreamOnce>::Position,
    >,
>
where
    Input: Stream<Token = Token>,
    Input::Position: Default,
    Input::Range: PartialEq,
{
    program().easy_parse(input)
}
