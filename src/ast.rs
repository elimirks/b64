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

#[derive(Clone)]
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
            Err(err)     => Err(CompErr {
                pos: None,
                message: err.to_string(),
            }),
        }
    }
}

pub trait GetPos {
    fn pos(&self) -> Pos;
}

pub struct RootStatements {
    pub functions: Vec<RSFunction>,
    pub variables: Vec<RSVariable>,
    pub imports: Vec<RSImport>,
}

impl RootStatements {
    pub fn new() -> RootStatements {
        RootStatements {
            functions: vec!(),
            variables: vec!(),
            imports: vec!(),
        }
    }
}

pub struct RSFunction {
    pub pos: Pos,
    pub name: String,
    pub args: Vec<String>,
    pub body: Statement,
}

pub struct RSVariable {
    pub pos: Pos,
    pub var: Var,
}

pub struct RSImport {
    pub pos: Pos,
    pub path: String,
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
    // The second parameter is the label id: (file_no, str_index)
    // The actual string raw data is stored separately, so we can
    // easily allocate it in the data segment
    Str(Pos, (usize, usize)),
    Call(Pos, String, Vec<Expr>),
    Int(Pos, i64),
    Assignment(Pos, String, Box<Expr>),
    DerefAssignment(Pos, Box<Expr>, Box<Expr>),
    UnaryOperator(Pos, UnaryOp, Box<Expr>),
    BinOperator(Pos, BinOp, Box<Expr>, Box<Expr>),
    // (cond, true_expr, false_expr)
    Cond(Pos, Box<Expr>, Box<Expr>, Box<Expr>),
    Reference(Pos, String),
    Dereference(Pos, Box<Expr>),
}
 
impl GetPos for Expr {
    fn pos(&self) -> Pos {
        match self {
            Expr::Id(pos, _)                 => pos.clone(),
            Expr::Str(pos, _)                => pos.clone(),
            Expr::Call(pos, _, _)            => pos.clone(),
            Expr::Int(pos, _)                => pos.clone(),
            Expr::Assignment(pos, _, _)      => pos.clone(),
            Expr::DerefAssignment(pos, _, _) => pos.clone(),
            Expr::UnaryOperator(pos, _, _)   => pos.clone(),
            Expr::BinOperator(pos, _, _, _)  => pos.clone(),
            Expr::Reference(pos, _)          => pos.clone(),
            Expr::Dereference(pos, _)        => pos.clone(),
            Expr::Cond(pos, _, _, _)         => pos.clone(),
        }
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum BinOp {
    Assign(Option<Box<BinOp>>), // FIXME: This shouldn't need to be heap allocated
    Add,
    Sub,
    Div,
    Mod,
    Mul,
    Eq,
    Ne,
    Le,
    Ge,
    Lt,
    Gt,
    ShiftRight,
    ShiftLeft,
    And,
    Or,
    Xor,
}

impl BinOp {
    pub fn assign(nested: BinOp) -> BinOp {
        BinOp::Assign(Some(Box::new(nested)))
    }
}

#[derive(Debug, Clone, PartialEq)]
pub enum UnaryOp {
    Increment,
    Decrement,
    Negate,
    BitNot,
}

impl BinOp {
    #[allow(dead_code)]
    pub fn is_comparison(&self) -> bool {
        match self {
            BinOp::Eq => true,
            BinOp::Ne => true,
            BinOp::Le => true,
            BinOp::Ge => true,
            BinOp::Lt => true,
            BinOp::Gt => true,
            _      => false,
        }
    }
}
