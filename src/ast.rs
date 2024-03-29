#[derive(Debug, Clone)]
pub struct Pos {
    pub offset: usize,
    pub file_id: usize,
}

impl Pos {
    pub fn new(offset: usize, file_id: usize) -> Pos {
        Pos {
            offset,
            file_id,
        }
    }
}

#[derive(Clone, Debug)]
pub struct CompErr {
    pub pos: Option<Pos>,
    pub message: String,
}

impl CompErr {
    pub fn err<T>(pos: &Pos, message: String) -> Result<T, CompErr> {
        Err(CompErr {
            pos: Some(pos.clone()),
            message,
        })
    }

    pub fn from_io_res<T>(io_res: Result<T, std::io::Error>) -> Result<T, CompErr> {
        match io_res {
            Ok(result) => Ok(result),
            Err(err) => Err(CompErr {
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
    pub defines: Vec<RSDefine>,
}

impl RootStatements {
    pub fn new() -> RootStatements {
        RootStatements {
            functions: vec![],
            variables: vec![],
            imports: vec![],
            defines: vec![],
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

pub struct RSDefine {
    pub pos: Pos,
    pub name: String,
    pub args: Vec<String>,
    pub body: Expr,
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
    Switch(Expr, Vec<SwInner>),
}

/// Statements allowed inside switch statements
/// Switch needs special rules for "case" and "default"
#[derive(Debug)]
pub enum SwInner {
    Default(Pos),
    Case(Pos, i64),
    Statement(Statement),
}

#[derive(Debug)]
// Singles & Vecs may have optional initial values
pub enum Var {
    // (name, capacity, values)
    // values.len() doesn't have to equal the capacity!
    Vec(String, i64, Vec<i64>),
    // (name, value)
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

#[derive(Clone)]
pub enum Expr {
    Id(Pos, String),
    // The second parameter is the unique string ID
    // The actual string raw data is stored separately, so we can
    // easily allocate it in the data segment
    Str(Pos, usize),
    Call(Pos, Box<Expr>, Vec<Expr>),
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
            Expr::Id(pos, _) => pos.clone(),
            Expr::Str(pos, _) => pos.clone(),
            Expr::Call(pos, _, _) => pos.clone(),
            Expr::Int(pos, _) => pos.clone(),
            Expr::Assignment(pos, _, _) => pos.clone(),
            Expr::DerefAssignment(pos, _, _) => pos.clone(),
            Expr::UnaryOperator(pos, _, _) => pos.clone(),
            Expr::BinOperator(pos, _, _, _) => pos.clone(),
            Expr::Reference(pos, _) => pos.clone(),
            Expr::Dereference(pos, _) => pos.clone(),
            Expr::Cond(pos, _, _, _) => pos.clone(),
        }
    }
}

impl std::fmt::Debug for Expr {
    fn fmt(&self, f: &mut std::fmt::Formatter<'_>) -> std::fmt::Result {
        match self {
            Expr::Id(_, id) => write!(f, "Id(_, {id})"),
            Expr::Str(_, str_id) => write!(f, "Str(_, {str_id})"),
            Expr::Call(_, fun_expr, arg_exprs) => write!(f, "Call(_, {:?}, {:?})", fun_expr, arg_exprs),
            Expr::Int(_, value) => write!(f, "Int(_, {value})"),
            Expr::Assignment(_, id, expr) => write!(f, "Assignment(_, {id}, {:?})", expr),
            Expr::DerefAssignment(_, lhs, rhs) => write!(f, "DerefAssignment(_, {:?}, {:?})", lhs, rhs),
            Expr::UnaryOperator(_, op, rhs) => write!(f, "UnaryOperator(_, {:?}, {:?})", op, rhs),
            Expr::BinOperator(_, op, lhs, rhs) => write!(f, "BinOperator(_, {:?}, {:?}, {:?})", op, lhs, rhs),
            Expr::Cond(_, cond_expr, t_expr, f_expr) => write!(f, "Cond(_, {:?}, {:?}, {:?})", cond_expr, t_expr, f_expr),
            Expr::Reference(_, id) => write!(f, "Reference(_, {id})"),
            Expr::Dereference(_, expr) => write!(f, "Dereference(_, {:?})", expr),
        }
    }
}

#[derive(Debug, Clone, Eq, PartialEq)]
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

#[derive(Debug, Clone, Eq, PartialEq)]
pub enum UnaryOp {
    PreIncrement,
    PreDecrement,
    PostIncrement,
    PostDecrement,
    Negate,
    BitNot,
}

impl BinOp {
    #[allow(dead_code)]
    pub fn is_comparison(&self) -> bool {
        matches!(
            self,
            BinOp::Eq | BinOp::Ne | BinOp::Le | BinOp::Ge | BinOp::Lt |
            BinOp::Gt
        )
    }
}
