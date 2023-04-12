use crate::expr::{DecoratedExpr, Expr, Identifier, Type};

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub name: Identifier,
    pub type_: Type,
    pub args: Vec<Identifier>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefn<T> {
    pub declaration: FunctionDecl,
    pub body: DecoratedExpr<T>,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement<T> {
    ExprStatement(DecoratedExpr<T>),
    FunctionDeclStatement(FunctionDecl),
    FunctionDefnStatement(FunctionDefn<T>),
}

pub type Program<T> = Vec<Statement<T>>;
