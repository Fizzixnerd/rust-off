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
    Ptr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Adr,
}

pub type Identifier = String;

#[derive(Debug, Clone, PartialEq)]
pub struct Type {
    pub modifiers: Vec<TypeModifier>,
    pub value_type: ValueType,
}

#[derive(Debug, Clone, PartialEq)]
pub enum TypeModifier {
    Pointer,
    Array(usize),
}

#[derive(Debug, Clone, PartialEq)]
pub enum ValueType {
    Int,
    USize,
    Double,
    Char,
    Bool,
    Function(Box<Type>, Vec<Type>),
    //    MemPtr,
}

#[derive(Debug, Clone, PartialEq)]
pub enum Atom {
    Int(i64),
    USize(usize),
    Double(f64),
    Char(i8),
    Bool(bool),
    Identifier(Identifier),
}

#[derive(Debug, Clone, PartialEq)]
pub enum Expr<T> {
    UnaryExpr(UnaryOp, Box<DecoratedExpr<T>>),
    BinaryExpr(BinaryOp, Box<DecoratedExpr<T>>, Box<DecoratedExpr<T>>),
    FuncallExpr(Identifier, Vec<DecoratedExpr<T>>),
    ParensExpr(Box<DecoratedExpr<T>>),
    AtomicExpr(Atom),
    IfThenElseExpr(
        Box<DecoratedExpr<T>>,
        Box<DecoratedExpr<T>>,
        Box<DecoratedExpr<T>>,
    ),
}

#[derive(Debug, Clone, PartialEq)]
pub struct DecoratedExpr<T> {
    pub expr: Expr<T>,
    pub decoration: T,
}

pub type TypedExpr = DecoratedExpr<Type>;
