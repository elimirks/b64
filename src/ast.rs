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
    Null, // "no op" essentially
    Auto(Vec<String>),
    Block(Vec<Statement>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
}

#[derive(Debug)]
pub enum Expr {
    Id(String),
    Call(String, Vec<Expr>),
    Int(i64),
    Assignment(String, Box<Expr>),
    Operator(Op, Box<Expr>, Box<Expr>),
    // App(String, Vec<Box<Expr>>),
    // Var(String),
}

#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
    Equals,
    Le,
    Ge,
    Lt,
    Gt,
}

impl Op {
    #[allow(dead_code)]
    pub fn is_comparison(&self) -> bool {
        match self {
            Op::Equals => true,
            Op::Le     => true,
            Op::Ge     => true,
            Op::Lt     => true,
            Op::Gt     => true,
            _      => false,
        }
    }
}
