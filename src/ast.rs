#[derive(Debug)]
pub enum RootStatement {
    Function(String, Vec<String>, Statement),
}

#[derive(Debug)]
pub enum Statement {
    // Statement representing executing a single expr
    Expr(Expr),
    ReturnExpr(Expr),
    Return,
    Break,
    Null, // "no op" essentially
    Label(String),
    Goto(String),
    Auto(Vec<String>),
    Block(Vec<Statement>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
}

#[derive(Debug)]
pub enum Expr {
    Id(String),
    Call(String, Vec<Expr>),
    Int(i64),
    Assignment(String, Box<Expr>),
    Operator(Op, Box<Expr>, Box<Expr>),
}

#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
}

impl Op {
    #[allow(dead_code)]
    pub fn is_comparison(&self) -> bool {
        match self {
            Op::Eq => true,
            Op::Ne => true,
            Op::Le => true,
            Op::Ge => true,
            Op::Lt => true,
            Op::Gt => true,
            _      => false,
        }
    }
}
