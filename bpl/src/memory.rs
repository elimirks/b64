use std::fmt;

#[allow(dead_code)]
#[derive(Clone, Copy, PartialEq, Hash, Eq)]
pub enum Reg {
    Rax, Rbx, Rcx, Rdx,
    Rdi, Rsi, Rbp, Rsp,
    R8,  R9,  R10, R11,
    R12, R13, R14, R15,
}

// Caller save registers, except for %rsp
pub const USABLE_CALLER_SAVE_REG: &'static [Reg] = &[
    Reg::Rax, Reg::Rcx, Reg::Rdx, Reg::Rdi,
    Reg::Rsi, Reg::R8,  Reg::R9,  Reg::R10,
    Reg::R11,
];

// Where values are located
#[derive(Clone, Copy, PartialEq)]
pub enum Loc {
    // Stack position relative to %rbp
    // +1 means the return address, +2 means 7th arg, +3 means 8th, ...
    // As per x86_64 Linux calling conventions, the
    // first 6 args are passed into functions by registers
    Stack(i64),
    Register(Reg),
    // "immediate" literal int values
    Immediate(i64),
}

impl fmt::Display for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Reg::Rax => write!(f, "rax"),
            Reg::Rbx => write!(f, "rbx"),
            Reg::Rcx => write!(f, "rcx"),
            Reg::Rdx => write!(f, "rdx"),
            Reg::Rdi => write!(f, "rdi"),
            Reg::Rsi => write!(f, "rsi"),
            Reg::Rbp => write!(f, "rbp"),
            Reg::Rsp => write!(f, "rsp"),
            Reg::R8  => write!(f, "r8"),
            Reg::R9  => write!(f, "r9"),
            Reg::R10 => write!(f, "r10"),
            Reg::R11 => write!(f, "r11"),
            Reg::R12 => write!(f, "r12"),
            Reg::R13 => write!(f, "r13"),
            Reg::R14 => write!(f, "r14"),
            Reg::R15 => write!(f, "r15"),
        }
    }
}

impl fmt::Debug for Reg {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Loc::Stack(offset)    => write!(f, "{}(%rbp)", 8 * offset),
            Loc::Register(reg)    => write!(f, "%{}", reg),
            Loc::Immediate(value) => write!(f, "${}", value),
        }
    }
}

impl fmt::Debug for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}
