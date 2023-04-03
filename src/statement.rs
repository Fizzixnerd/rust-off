use crate::expr::Expr;

pub type FunctionType = usize;

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDecl {
    pub name: String,
    pub type_: FunctionType,
    pub args: Vec<String>,
}

#[derive(Debug, Clone, PartialEq)]
pub struct FunctionDefn {
    pub declaration: FunctionDecl,
    pub body: Expr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Statement {
    ExprStatement(Expr),
    FunctionDeclStatement(FunctionDecl),
    FunctionDefnStatement(FunctionDefn),
}

pub type Program = Vec<Statement>;
