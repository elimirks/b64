use std::fmt;
use std::convert;

#[allow(dead_code)]
#[derive(Clone, Copy, Eq, PartialEq)]
pub enum Reg {
    Rax = 0b0000000000000001,
    Rbx = 0b0000000000000010,
    Rcx = 0b0000000000000100,
    Rdx = 0b0000000000001000,
    Rdi = 0b0000000000010000,
    Rsi = 0b0000000000100000,
    Rbp = 0b0000000001000000,
    Rsp = 0b0000000010000000,
    R8 = 0b0000000100000000,
    R9 = 0b0000001000000000,
    R10 = 0b0000010000000000,
    R11 = 0b0000100000000000,
    R12 = 0b0001000000000000,
    R13 = 0b0010000000000000,
    R14 = 0b0100000000000000,
    R15 = 0b1000000000000000,
}

impl Reg {
    /// References:
    /// * http://ref.x86asm.net/coder64.html
    pub fn opcode_id(&self) -> u8 {
        match self {
            Reg::Rax => 0,
            Reg::Rbx => 3,
            Reg::Rcx => 1,
            Reg::Rdx => 2,
            Reg::Rdi => 7,
            Reg::Rsi => 6,
            Reg::Rbp => 5,
            Reg::Rsp => 4,
            // These are "extended" registers, be sure to check is_ext!
            Reg::R8  => 0,
            Reg::R9  => 1,
            Reg::R10 => 2,
            Reg::R11 => 3,
            Reg::R12 => 4,
            Reg::R13 => 5,
            Reg::R14 => 6,
            Reg::R15 => 7,
        }
    }

    /// Is this register an extended register?
    /// Extended registers often have different opcodes!
    pub fn is_ext(&self) -> bool {
        match self {
            Reg::Rax => false,
            Reg::Rbx => false,
            Reg::Rcx => false,
            Reg::Rdx => false,
            Reg::Rdi => false,
            Reg::Rsi => false,
            Reg::Rbp => false,
            Reg::Rsp => false,
            Reg::R8  => true,
            Reg::R9  => true,
            Reg::R10 => true,
            Reg::R11 => true,
            Reg::R12 => true,
            Reg::R13 => true,
            Reg::R14 => true,
            Reg::R15 => true,
        }
    }

    /// Get the extended low byte for this register
    /// IMPORTANT: It returns the RAX for the low byte
    pub fn low_byte(&self) -> &str {
        match self {
            Reg::Rax => "al",
            Reg::Rbx => "bl",
            Reg::Rcx => "cl",
            Reg::Rdx => "dl",
            Reg::Rdi => "dil",
            Reg::Rsi => "sil",
            Reg::Rbp => "bpl",
            Reg::Rsp => "spl",
            Reg::R8 => "r8b",
            Reg::R9 => "r9b",
            Reg::R10 => "r10b",
            Reg::R11 => "r11b",
            Reg::R12 => "r12b",
            Reg::R13 => "r13b",
            Reg::R14 => "r14b",
            Reg::R15 => "r15b",
        }
    }

    pub fn for_arg_num(num: usize) -> Reg {
        match num {
            0 => Reg::Rdi,
            1 => Reg::Rsi,
            2 => Reg::Rdx,
            3 => Reg::Rcx,
            4 => Reg::R8,
            5 => Reg::R9,
            _ => {
                panic!("arg num {} is not stored in a register", num)
            }
        }
    }

    pub fn for_syscall_arg_num(num: usize) -> Reg {
        match num {
            0 => Reg::Rax, // Syscall number
            // The rest are considered arguments
            1 => Reg::Rdi,
            2 => Reg::Rsi,
            3 => Reg::Rdx,
            4 => Reg::R10,
            5 => Reg::R8,
            6 => Reg::R9,
            _ => {
                panic!("arg num {} is not stored in a register", num)
            }
        }
    }

    // Expects the mask to only represent ONE register
    fn from_u16(mask: u16) -> Reg {
        match mask {
            0b0000000000000001 => Reg::Rax,
            0b0000000000000010 => Reg::Rbx,
            0b0000000000000100 => Reg::Rcx,
            0b0000000000001000 => Reg::Rdx,
            0b0000000000010000 => Reg::Rdi,
            0b0000000000100000 => Reg::Rsi,
            0b0000000001000000 => Reg::Rbp,
            0b0000000010000000 => Reg::Rsp,
            0b0000000100000000 => Reg::R8,
            0b0000001000000000 => Reg::R9,
            0b0000010000000000 => Reg::R10,
            0b0000100000000000 => Reg::R11,
            0b0001000000000000 => Reg::R12,
            0b0010000000000000 => Reg::R13,
            0b0100000000000000 => Reg::R14,
            0b1000000000000000 => Reg::R15,
            _ => panic!("{} is not a valid register mask", mask),
        }
    }
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
            Reg::R8 => write!(f, "r8"),
            Reg::R9 => write!(f, "r9"),
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


// Efficiently represents a set of registers
#[derive(Clone)]
pub struct RegSet {
    bitmask: u16,
}

impl RegSet {
    pub fn empty() -> RegSet {
        RegSet { bitmask: 0 }
    }

    pub fn of(reg: Reg) -> RegSet {
        RegSet {
            bitmask: reg as u16,
        }
    }

    pub fn usable_caller_save() -> RegSet {
        let registers = [
            Reg::Rax,
            Reg::Rcx,
            Reg::Rdx,
            Reg::Rdi,
            Reg::Rsi,
            Reg::R8,
            Reg::R9,
            Reg::R10,
            Reg::R11,
        ];

        let mut mask = 0;
        for reg in registers {
            mask |= reg as u16;
        }

        RegSet { bitmask: mask }
    }

    pub fn contains(&self, reg: Reg) -> bool {
        (self.bitmask & (reg as u16)) != 0
    }

    pub fn union(&self, other: RegSet) -> RegSet {
        RegSet {
            bitmask: self.bitmask | other.bitmask,
        }
    }

    pub fn subtract(&self, other: &RegSet) -> RegSet {
        RegSet {
            bitmask: self.bitmask & !other.bitmask,
        }
    }

    pub fn with(&self, reg: Reg) -> RegSet {
        let reg_mask = reg as u16;
        RegSet {
            bitmask: self.bitmask | reg_mask,
        }
    }

    pub fn first(&self) -> Option<Reg> {
        if self.bitmask == 0 {
            None
        } else {
            let lowest_bit = self.bitmask & (!self.bitmask + 1);
            Some(Reg::from_u16(lowest_bit))
        }
    }
}

// Where values are located
#[derive(Clone, PartialEq, Eq)]
pub enum Loc {
    // Stack position relative to %rbp
    // +1 means the return address, +2 means 7th arg, +3 means 8th, ...
    // As per x86_64 Linux calling conventions, the
    // first 6 args are passed into functions by registers
    Stack(i64),
    // Stored in the data segment with the given label
    Data(String),
    Register(Reg),
    // "immediate" literal int values
    Immediate(i64),
    // Indirect mode. For the form (%register)
    Indirect(Reg),
}

impl Loc {
    pub fn is_reg(&self) -> bool {
        matches!(self, Loc::Register(_))
    }

    #[allow(dead_code)]
    pub fn is_mem(&self) -> bool {
        matches!(self, Loc::Stack(_) | Loc::Data(_) | Loc::Indirect(_))
    }
}

impl fmt::Display for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        match self {
            Loc::Stack(offset) => write!(f, "{}(%rbp)", 8 * offset),
            Loc::Register(reg) => write!(f, "%{}", reg),
            Loc::Immediate(value) => write!(f, "${}", value),
            Loc::Data(name) => write!(f, "{}(%rip)", name),
            Loc::Indirect(reg) => write!(f, "(%{})", reg),
        }
    }
}

impl fmt::Debug for Loc {
    fn fmt(&self, f: &mut fmt::Formatter) -> fmt::Result {
        write!(f, "{}", self)
    }
}

impl convert::From<Reg> for Loc {
    fn from(reg: Reg) -> Loc {
        Loc::Register(reg)
    }
}

impl convert::From<i64> for Loc {
    fn from(value: i64) -> Loc {
        Loc::Immediate(value)
    }
}
