#[derive(Debug, Clone)]
pub struct Pos {
    pub offset: usize,
    pub file_id: usize,
}

impl Pos {
    pub fn new(offset: usize, file_id: usize) -> Pos {
        Pos {
            offset: offset,
            file_id: file_id,
        }
    }
}

pub struct CompErr {
    pub pos: Option<Pos>,
    pub message: String,
}

impl CompErr {
    pub fn err<T>(pos: &Pos, message: String) -> Result<T, CompErr> {
        Err(CompErr {
            pos: Some(pos.clone()),
            message: message,
        })
    }

    pub fn from_io_res<T>(
        io_res: Result<T, std::io::Error>
    ) -> Result<T, CompErr> {
        match io_res {
            Ok(result) => Ok(result),
            Err(_)     => Err(CompErr {
                pos: None,
                message: "Failed writing to output".to_string(),
            }),
        }
    }
}

pub trait GetPos {
    fn pos(&self) -> Pos;
}

#[derive(Debug)]
pub enum RootStatement {
    Function(Pos, String, Vec<String>, Statement),
    Variable(Pos, Var),
}

#[derive(Debug)]
pub enum Statement {
    // Statement representing executing a single expr
    Expr(Expr),
    ReturnExpr(Expr),
    Return,
    Break(Pos),
    Null, // "no op" essentially
    Label(Pos, String),
    Goto(Pos, String),
    Auto(Pos, Vec<Var>),
    Extern(Pos, Vec<String>),
    Block(Vec<Statement>),
    If(Expr, Box<Statement>, Option<Box<Statement>>),
    While(Expr, Box<Statement>),
}

#[derive(Debug)]
// Singles & Vecs may have optional initial values
pub enum Var {
    Vec(String, i64, Vec<i64>),
    Single(String, Option<i64>),
}

impl Var {
    pub fn name(&self) -> &String {
        match self {
            Var::Vec(name, _, _) => name,
            Var::Single(name, _) => name,
        }
    }
}

#[derive(Debug, Clone)]
pub enum Expr {
    Id(Pos, String),
    Call(Pos, String, Vec<Expr>),
    Int(Pos, i64),
    Assignment(Pos, String, Box<Expr>),
    DerefAssignment(Pos, Box<Expr>, Box<Expr>),
    Operator(Pos, Op, Box<Expr>, Box<Expr>),
    Reference(Pos, String),
    Dereference(Pos, Box<Expr>),
}

impl GetPos for Expr {
    fn pos(&self) -> Pos {
        match self {
            Expr::Id(pos, _)                 => pos.clone(),
            Expr::Call(pos, _, _)            => pos.clone(),
            Expr::Int(pos, _)                => pos.clone(),
            Expr::Assignment(pos, _, _)      => pos.clone(),
            Expr::DerefAssignment(pos, _, _) => pos.clone(),
            Expr::Operator(pos, _, _, _)     => pos.clone(),
            Expr::Reference(pos, _)          => pos.clone(),
            Expr::Dereference(pos, _)        => pos.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum Op {
    Assign,
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
