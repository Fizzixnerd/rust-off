#[derive(Debug, Clone, PartialEq)]
pub enum BinaryOp {
    Add,
    Sub,
    Mul,
    Div,
    Lt,
    Gt,
    Eq,
    Neq,
}

pub type Identifier = String;

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Float(f64),
    Identifier(String),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr {
    BinaryExpr(BinaryOp, Box<Expr>, Box<Expr>),
    FuncallExpr(Identifier, Vec<Expr>),
    ParensExpr(Box<Expr>),
    AtomicExpr(Atom),
    IfThenElseExpr(Box<Expr>, Box<Expr>, Box<Expr>),
}
