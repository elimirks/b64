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
    Auto(Vec<Auto>),
    Block(Vec<Statement>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
}

#[derive(Debug)]
pub enum Auto {
    Vec(String, i64),
    Val(String),
}

impl Auto {
    pub fn name(&self) -> &String {
        match self {
            Auto::Vec(name, _) => name,
            Auto::Val(name)    => name,
        }
    }
}

#[derive(Debug)]
pub enum Expr {
    Id(String),
    Call(String, Vec<Expr>),
    Int(i64),
    Assignment(String, Box<Expr>),
    DerefAssignment(Box<Expr>, Box<Expr>),
    Operator(Op, Box<Expr>, Box<Expr>),
    Reference(String),
    Dereference(Box<Expr>),
}

#[derive(Debug)]
pub enum Op {
    Add,
    Sub,
    Div,
    Mod,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
    ShiftRight,
    ShiftLeft,
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
