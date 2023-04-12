use combine::easy::Errors;
use combine::stream::StreamErrorFor;
use combine::{
    attempt, choice, many1, parser, position, satisfy, satisfy_map, sep_by, value, EasyParser,
    ParseError, Parser, Stream, StreamOnce,
};

use crate::expr::{Atom, BinaryOp, DecoratedExpr, Expr, TypeModifier, UnaryOp, ValueType};
use crate::statement::{FunctionDecl, FunctionDefn, Program, Statement};
use crate::token::Token;

fn match_binary_operator(t: Token) -> Option<BinaryOp> {
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

fn match_unary_operator(t: Token) -> Option<UnaryOp> {
    match t {
        Token::Operator(unop) => match unop.as_str() {
            "&" => Some(UnaryOp::Adr),
            _ => None,
        },
        _ => None,
    }
}

fn boperator<Input>() -> impl Parser<Input, Output = BinaryOp>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    satisfy_map(match_binary_operator)
}

fn uoperator<Input>() -> impl Parser<Input, Output = UnaryOp>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    satisfy_map(match_unary_operator)
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

fn binary_expr<Input>() -> impl Parser<Input, Output = DecoratedExpr<()>>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt(expr_nonlrec().and(boperator()))
        .and(expr())
        .map(|((e1, op), e2)| Expr::BinaryExpr(op, Box::new(e1), Box::new(e2)))
        .map(|e| DecoratedExpr {
            expr: e,
            decoration: (),
        })
}

fn unary_expr<Input>() -> impl Parser<Input, Output = DecoratedExpr<()>>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt(uoperator())
        .and(expr())
        .map(|(op, e)| Expr::UnaryExpr(op, Box::new(e)))
        .map(|e| DecoratedExpr {
            expr: e,
            decoration: (),
        })
}

fn match_atom(t: Token) -> Option<Atom> {
    match t {
        Token::Floating(f) => Some(Atom::Double(f)),
        Token::USize(u) => Some(Atom::USize(u)),
        Token::Identifier(s) => Some(Atom::Identifier(s)),
        _ => None,
    }
}

fn atomic_expr<Input>() -> impl Parser<Input, Output = DecoratedExpr<()>>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    satisfy_map(match_atom)
        .map(Expr::AtomicExpr)
        .map(|e| DecoratedExpr {
            expr: e,
            decoration: (),
        })
}

fn eq_token<Input>(t: Token) -> impl Parser<Input, Output = Token>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    satisfy(move |s| s == t)
}

fn parens_expr<Input>() -> impl Parser<Input, Output = DecoratedExpr<()>>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    eq_token(Token::LParen)
        .with(expr())
        .skip(eq_token(Token::RParen))
        .map(|e| Expr::ParensExpr(Box::new(e)))
        .map(|e| DecoratedExpr {
            expr: e,
            decoration: (),
        })
}

fn funcall_expr<Input>() -> impl Parser<Input, Output = DecoratedExpr<()>>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt(identifier().skip(eq_token(Token::LParen)))
        .and(sep_by(expr(), eq_token(Token::Comma)))
        .skip(eq_token(Token::RParen))
        .map(|(i, es)| Expr::FuncallExpr(i, es))
        .map(|e| DecoratedExpr {
            expr: e,
            decoration: (),
        })
}

fn if_then_else_expr<Input>() -> impl Parser<Input, Output = DecoratedExpr<()>>
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
        .map(|e| DecoratedExpr {
            expr: e,
            decoration: (),
        })
}

parser! {
    fn expr_nonlrec[Input]()(Input) -> DecoratedExpr<()>
    where [Input: Stream<Token = Token>]
    {
        choice!(unary_expr(), if_then_else_expr(), parens_expr(), funcall_expr(), atomic_expr())
    }
}

parser! {
    fn expr[Input]()(Input) -> DecoratedExpr<()>
    where [Input: Stream<Token = Token>]
    {
       choice!(binary_expr(), expr_nonlrec())
    }
}

fn expr_statement<Input>() -> impl Parser<Input, Output = Statement<()>>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    expr().map(Statement::ExprStatement)
}

fn value_type<Input>() -> impl Parser<Input, Output = ValueType>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    satisfy_map(match_type_identifier)
}

fn match_type_identifier(t: Token) -> Option<ValueType> {
    match t {
        Token::TypeIdentifier(ty) => match ty.as_str() {
            "Int" => Some(ValueType::Int),
            "USize" => Some(ValueType::USize),
            "Double" => Some(ValueType::Double),
            "Char" => Some(ValueType::Char),
            "Bool" => Some(ValueType::Bool),
            _ => None,
        },
        _ => None,
    }
}

fn match_type_modifier(t: Token) -> Option<TypeModifier> {
    match t {
        Token::TypeIdentifier(ty) => match ty.as_str() {
            "Pointer" => Some(TypeModifier::Pointer),
            "Array" => Some(TypeModifier::Array(0)),
            _ => None,
        },
        _ => None,
    }
}

fn match_usize_literal(t: Token) -> Option<usize> {
    match t {
        Token::USize(u) => Some(u),
        _ => None,
    }
}

fn type_modifier<Input>() -> impl Parser<Input, Output = TypeModifier>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    satisfy_map(match_type_modifier).then(|modifier| match modifier {
        TypeModifier::Pointer => value(TypeModifier::Pointer),
        TypeModifier::Array(_) => eq_token(Token::LParen)
            .with(satisfy_map(match_usize_literal))
            .skip(eq_token(Token::RParen))
            .map(TypeModifier::Array),
    })
}

fn manifest_type<Input>() -> impl Parser<Input, Output = Type>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
}

fn function_decl<Input>() -> impl Parser<Input, Output = FunctionDecl>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    attempt(identifier().skip(eq_token(Token::LParen)))
        .and(sep_by::<Vec<String>, _, _, _>(
            identifier()
                .skip(eq_token(Token::Colon))
                .and(manifest_type()),
            eq_token(Token::Comma),
        ))
        .skip(eq_token(Token::RParen))
        .map(|(name, args)| FunctionDecl {
            name,
            type_: args.len(),
            args,
        })
}

fn function_decl_statement<Input>() -> impl Parser<Input, Output = Statement<()>>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    eq_token(Token::Extern)
        .with(function_decl())
        .map(Statement::FunctionDeclStatement)
}

fn function_defn_statement<Input>() -> impl Parser<Input, Output = Statement<()>>
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

pub fn statement<Input>() -> impl Parser<Input, Output = Statement<()>>
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

pub fn program<Input>() -> impl Parser<Input, Output = Program<()>>
where
    Input: Stream<Token = Token>,
    Input::Error: ParseError<Input::Token, Input::Range, Input::Position>,
{
    many1(statement())
}

pub fn parse<Input>(
    input: Input,
) -> Result<
    (Program<()>, Input),
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
