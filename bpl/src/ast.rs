#[derive(Debug)]
pub enum RootStatement {
    Function(String, Vec<String>, Statement),
}

#[derive(Debug)]
pub enum Statement {
    // Statement representing executing a single expr
    Expr(Expr),
    Return(Expr),
    Auto(Vec<String>),
    Block(Vec<Statement>),
}

#[derive(Debug)]
pub enum Expr {
    Id(String),
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
}
