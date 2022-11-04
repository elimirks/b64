use std::collections::HashMap;
use std::collections::HashSet;
use std::convert::TryInto;
use std::io::BufWriter;
use std::io::Write;
use std::sync::Condvar;
use std::sync::{Arc, Mutex};
use std::thread;

use crate::ast::*;
use crate::memory::*;
use crate::parser::*;
use crate::util::logical_cpu_count;

// Represents an instruction in both ASM and Op Codes (WIP)
type Inst = Vec<u8>;

#[derive(PartialEq)]
enum Jump {
    Jmp,
    Jne,
    Je,
    Jle,
    Jl,
    Jge,
    Jg,
}

const PAGE_SIZE: i64 = 0x1000;
const ELF_HEADER_SIZE: i64 = 0x40;
const PROGRAM_HEADER_SIZE: i64 = 0x38;
const PROGRAM_HEADER_COUNT: i64 = 3;
const SECTION_HEADER_SIZE: i64 = 0x40;
const SECTION_HEADER_COUNT: i64 = 5;
const SHTAB_SECTION_INDEX: i64 = 1;
const VADDR_BASE: i64 = 0x010000;

#[derive(Debug)]
struct ElfContext {
    program_size: i64,
    data_size: i64,
    rodata_size: i64,
    shstrtab_size: i64,
}

impl ElfContext {
    fn program_vaddr(&self) -> i64 {
        self.data_vaddr() + PAGE_SIZE * self.data_page_num()
    }

    fn program_offset(&self) -> i64 {
        self.data_offset() + PAGE_SIZE * self.data_page_num()
    }

    fn data_vaddr(&self) -> i64 {
        self.rodata_vaddr() + PAGE_SIZE * self.rodata_page_num()
    }

    fn data_page_num(&self) -> i64 {
        ceil_to_page_size(self.data_size) / PAGE_SIZE
    }

    fn data_offset(&self) -> i64 {
        self.rodata_offset() + PAGE_SIZE * self.rodata_page_num()
    }

    fn rodata_vaddr(&self) -> i64 {
        VADDR_BASE
    }

    fn rodata_page_num(&self) -> i64 {
        ceil_to_page_size(self.rodata_size) / PAGE_SIZE
    }

    fn rodata_offset(&self) -> i64 {
        PAGE_SIZE
    }

    fn section_header_offset(&self) -> i64 {
        self.program_offset() + self.program_size
    }

    fn shstrtab_offset(&self) -> i64 {
        ELF_HEADER_SIZE + PROGRAM_HEADER_SIZE * PROGRAM_HEADER_COUNT
    }
}

fn ceil_to_page_size(n: i64) -> i64 {
    let page_mask = PAGE_SIZE - 1;
    (n + page_mask) & (!page_mask)
}

/// Get the tail of an instruction (32 bytes) as a usize value
fn inst_32_tail(inst: &Inst) -> usize {
    u32::from_le_bytes(inst[inst.len() - 4..].try_into().unwrap()) as usize
}

// Calculate the byte offset for the given instruction index
fn inst_offset(
    instructions: &Vec<Inst>,
    mut inst_index: usize,
) -> i64 {
    let mut offset = 0;
    for inst in instructions {
        if inst_index == 0 {
            break;
        }
        offset += inst.len();
        inst_index -= 1;
    }
    offset as i64
}

struct CodeGenPool {
    running_fibers: usize,
    functions: Vec<RSFunction>,
    // Function name -> instructions, instructions to link
    results: Vec<(String, Vec<Inst>, Vec<(usize, String)>)>,
    errors: Vec<CompErr>,
}

#[derive(Debug, Clone)]
enum ScopeEntry {
    // Contains the number of args
    Fun(usize),
    Var(Loc),
    // First parameter are the macro args
    Define(Vec<String>, Expr),
}

struct FunContext<'a> {
    global_scope: &'a HashMap<String, ScopeEntry>,
    str_vaddrs: &'a HashMap<usize, i64>,
    data_vaddrs: &'a HashMap<String, i64>,
    /*
     * One HashMap for the entire function scope, for O(1) access
     * I thought about using a linked list of hashmaps...
     * But that's a lot of overhead
     */
    fun_scope: HashMap<String, ScopeEntry>,
    // Keeps track of the vars dedicated to the current block
    // So at the end of a block scope we can remove from the fun_scope
    block_vars: Vec<Vec<String>>,
    local_var_locs: HashMap<String, Loc>,
    // Maps from visible label name to compiled label name
    labels: HashMap<String, usize>,
    label_counter: usize,
    // Stack of end labels to tell break where to jump to
    break_dest_stack: Vec<usize>,
    // Indices of where each label points to in the instruction list
    // NOTE: This is the instruction index, not byte index (for now)
    label_indices: Vec<usize>,
    // List of instructions that require linking.
    // The linked instruction will always be last.
    // NOTE: There's a bug in here somewhere unfortunately...
    inst_links: Vec<(usize, String)>,
}

impl FunContext<'_> {
    fn new_label(&mut self) -> usize {
        let index = self.label_counter;
        // Actual value is set when the label is generated as an instruction
        self.label_indices.push(0);
        self.label_counter += 1;
        index
    }

    fn new_scope(&mut self) {
        self.block_vars.push(vec![]);
    }

    fn drop_scope(&mut self) {
        // We know it must exist, since a scope is pushed on function entry
        for var in self.block_vars.pop().unwrap() {
            self.fun_scope.remove(&var);
        }
    }

    fn add_to_scope(&mut self, pos: &Pos, name: String, entry: ScopeEntry) -> Result<(), CompErr> {
        if self.fun_scope.contains_key(&name) {
            return CompErr::err(pos, format!("{} is already in defined in this scope", name));
        }
        self.fun_scope.insert(name.clone(), entry);
        self.block_vars.last_mut().unwrap().push(name);
        Ok(())
    }

    fn find_in_scope(&self, name: &str) -> Option<&ScopeEntry> {
        if let Some(entry) = self.fun_scope.get(name) {
            return Some(entry);
        }
        match self.global_scope.get(name) {
            // Only allow referencing global vars when users specify "extrn"
            Some(ScopeEntry::Var(Loc::Data(_))) => None,
            other => other,
        }
    }
}

fn is_32_bounded(value: i64) -> bool {
    value < i32::MAX as i64 && value >= i32::MIN as i64
}

fn is_16_bounded(value: i64) -> bool {
    value < i16::MAX as i64 && value >= i16::MIN as i64
}

fn is_8_bounded(value: i64) -> bool {
    value < i8::MAX as i64 && value >= i8::MIN as i64
}

/**
 * Allocates stack memory for auto local_var_locs and finds labels.
 * @param body The function body to search for auto declarations
 * @param offset The positive offset from rbp (how much space came before this)
 */
fn prepass_gen(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    body: &Statement,
    offset: i64,
) -> Result<(), CompErr> {
    let mut stack = vec![body];
    let mut autos_size = 0;

    // DFS to find them all
    while !stack.is_empty() {
        match stack.pop().unwrap() {
            Statement::Label(pos, name) => {
                if c.labels.contains_key(name) {
                    return CompErr::err(
                        pos,
                        format!("Label {} already defined in this function", name),
                    );
                }
                let l = c.new_label();
                c.labels.insert(name.clone(), l);
            }
            Statement::Auto(pos, vars) => {
                for var in vars {
                    let name = var.name();

                    if c.local_var_locs.contains_key(name) {
                        return CompErr::err(
                            pos,
                            format!("{} already defined in this function", name),
                        );
                    }

                    let size = match var {
                        Var::Vec(_, vec_size, _) => 1 + vec_size,
                        Var::Single(_, _) => 1,
                    };

                    c.local_var_locs
                        .insert(name.clone(), Loc::Stack(-offset - autos_size));
                    autos_size += size;
                }
            }
            Statement::Block(statements) => {
                for s in statements {
                    stack.push(s);
                }
            }
            Statement::If(_, if_body, Some(else_body)) => {
                stack.push(if_body);
                stack.push(else_body);
            }
            Statement::If(_, body, None) => stack.push(body),
            Statement::While(_, body) => stack.push(body),
            _ => {}
        }
    }

    if autos_size > 0 {
        let immediate = 8 * autos_size;
        let mut opcodes = vec![0x48, 0x81, 0xec];
        append_le32_bytes(&mut opcodes, immediate);
        instructions.push(opcode_gen_arithmetic(c, &BinOp::Sub, &immediate.into(), &Reg::Rsp.into()));
    }

    Ok(())
}

fn append_le16_bytes(
    vector: &mut Vec<u8>,
    value: i64,
) {
    assert!(is_16_bounded(value));
    vector.push(value as u8);
    vector.push((value >> 8) as u8);
}

fn append_le32_bytes(
    vector: &mut Vec<u8>,
    value: i64,
) {
    assert!(is_32_bounded(value));
    vector.push(value as u8);
    vector.push((value >> 8) as u8);
    vector.push((value >> (8*2)) as u8);
    vector.push((value >> (8*3)) as u8);
}

fn append_le64_bytes(
    vector: &mut Vec<u8>,
    value: i64
) {
    vector.push(value as u8);
    vector.push((value >> 8) as u8);
    vector.push((value >> (8*2)) as u8);
    vector.push((value >> (8*3)) as u8);
    vector.push((value >> (8*4)) as u8);
    vector.push((value >> (8*5)) as u8);
    vector.push((value >> (8*6)) as u8);
    vector.push((value >> (8*7)) as u8);
}

fn opcode_gen_push(
    loc: &Loc,
) -> Inst {
    match loc {
        Loc::Stack(position) => {
            let offset = position * 8;

            let mut opcodes = vec![0xff];
            if offset >= i8::MIN as i64 && offset <= i8::MAX as i64 {
                opcodes.push(0x75);
                opcodes.push(offset as u8);
            } else {
                opcodes.push(0xb5);
                append_le32_bytes(&mut opcodes, offset);
            }
            opcodes
        },
        Loc::Data(_) => todo!(),
        Loc::Register(reg) => {
            if reg.is_ext() {
                vec![0x41, 0x50 + reg.opcode_id()]
            } else {
                vec![0x50 + reg.opcode_id()]
            }
        },
        Loc::Immediate(value) => {
            if !is_32_bounded(*value) {
                todo!("We don't support pushing 64 bit ints yet")
            }
            let mut opcodes = vec![0x68];
            append_le32_bytes(&mut opcodes, *value);
            opcodes
        },
        Loc::Indirect(_) => todo!(),
    }
}

fn opcode_gen_pop(
    loc: &Loc,
) -> Inst {
    match loc {
        Loc::Stack(_) => unreachable!(),
        Loc::Data(_) => unreachable!(),
        // The only valid opcodes are to pop into a register
        Loc::Register(reg) => {
            if reg.is_ext() {
                vec![0x41, 0x58 + reg.opcode_id()]
            } else {
                vec![0x58 + reg.opcode_id()]
            }
        },
        Loc::Immediate(_) => unreachable!(),
        Loc::Indirect(_) => todo!(),
    }
}

/// NOTE: You probably want to flip the order of rhs and lhs when calling this
fn opcode_gen_cmp(
    c: &FunContext,
    lhs_loc: &Loc,
    rhs_loc: &Loc,
) -> Inst {
    match (lhs_loc, rhs_loc) {
        (lhs, rhs) if lhs.is_mem() && rhs.is_mem() => unreachable!(),
        (Loc::Register(lhs_reg), Loc::Register(rhs_reg)) => {
            let mut opcodes = vec![match (lhs_reg.is_ext(), rhs_reg.is_ext()) {
                (true, true)   => 0x4d,
                (true, false)  => 0x4c,
                (false, true)  => 0x49,
                (false, false) => 0x48,
            }];
            opcodes.push(0x39);
            opcodes.push(0xc0 + (lhs_reg.opcode_id() << 3) + rhs_reg.opcode_id());
            opcodes
        },
        (Loc::Stack(lhs_index), Loc::Register(rhs_reg)) => {
            let mut opcodes = vec![match rhs_reg.is_ext() {
                true  => 0x4c,
                false => 0x48,
            }];
            opcodes.push(0x3b);

            let lhs_offset = lhs_index * 8;
            if is_8_bounded(lhs_offset) {
                opcodes.push(0x45 + (rhs_reg.opcode_id() << 3));
                opcodes.push(lhs_offset as u8);
            } else {
                opcodes.push(0x85 + (rhs_reg.opcode_id() << 3));
                append_le32_bytes(&mut opcodes, lhs_offset);
            }
            opcodes
        },
        (Loc::Register(_), Loc::Stack(_)) => {
            panic!("cmpq rhs should never be on the stack")
        },
        (Loc::Immediate(lhs_value), Loc::Register(rhs_reg)) => {
            let mut opcodes = vec![match rhs_reg.is_ext() {
                true  => 0x49,
                false => 0x48,
            }];
            if is_8_bounded(*lhs_value) {
                opcodes.push(0x83);
                opcodes.push(0xf8 + rhs_reg.opcode_id());
                opcodes.push(*lhs_value as u8);
            } else {
                // NOTE: There is a special case for rax, but I ignored it
                opcodes.push(0x81);
                opcodes.push(0xf8 + rhs_reg.opcode_id());
                append_le32_bytes(&mut opcodes, *lhs_value);
            }
            opcodes
        },
        (Loc::Immediate(lhs_value), Loc::Stack(rhs_index)) => {
            let mut opcodes = vec![0x48];
            let rhs_offset = rhs_index * 8;

            opcodes.push(if is_8_bounded(*lhs_value) { 0x83 } else { 0x81 });
            opcodes.push(if is_8_bounded(rhs_offset) { 0x7d } else { 0xbd });

            if is_8_bounded(*lhs_value) {
                opcodes.push(rhs_offset as u8);
            } else {
                append_le32_bytes(&mut opcodes, rhs_offset);
            }
            if is_8_bounded(rhs_offset) {
                opcodes.push(*lhs_value as u8);
            } else {
                append_le32_bytes(&mut opcodes, *lhs_value);
            }
            opcodes
        },
        (Loc::Data(name), Loc::Register(lhs_reg)) => {
            let vaddr = *c.data_vaddrs.get(name).unwrap();
            let mut opcodes = vec![if lhs_reg.is_ext() { 0x4c } else { 0x48 }];
            opcodes.push(0x3b);
            opcodes.push(0x04 + (lhs_reg.opcode_id() << 3));
            opcodes.push(0x25);
            append_le32_bytes(&mut opcodes, vaddr);
            opcodes
        },
        _ => unimplemented!()
    }
}

fn opcode_gen_mov(
    c: &FunContext,
    lhs_loc: &Loc,
    rhs_loc: &Loc,
) -> Inst {
    match (lhs_loc, rhs_loc) {
        (Loc::Immediate(imm), Loc::Register(rhs)) => {
            let mut opcodes = vec![match rhs.is_ext() {
                true  => 0x49,
                false => 0x48,
            }];
            if is_32_bounded(*imm) {
                opcodes.push(0xc7);
                opcodes.push(0xc0 + rhs.opcode_id());
                append_le32_bytes(&mut opcodes, *imm);
            } else {
                opcodes.push(0xb8 + rhs.opcode_id());
                append_le64_bytes(&mut opcodes, *imm);
            }
            opcodes
        },
        (Loc::Immediate(imm), Loc::Stack(stack_index)) => {
            let mut opcodes = vec![0x48, 0xc7];
            let stack_offset = stack_index * 8;
            if is_8_bounded(stack_offset) {
                opcodes.push(0x45);
                opcodes.push(stack_offset as u8);
            } else {
                opcodes.push(0x85);
                append_le32_bytes(&mut opcodes, stack_offset);
            }
            // TODO: Support the 64 bit case
            append_le32_bytes(&mut opcodes, *imm);
            opcodes
        },
        (Loc::Register(lhs), Loc::Register(rhs)) => {
            let mut opcodes = vec![match (lhs.is_ext(), rhs.is_ext()) {
                (true, true)   => 0x4d,
                (true, false)  => 0x4c,
                (false, true)  => 0x49,
                (false, false) => 0x48,
            }];
            opcodes.push(0x89);
            let rmbyte = 0b11000000 + (lhs.opcode_id() << 3) + rhs.opcode_id();
            opcodes.push(rmbyte);

            opcodes
        },
        (Loc::Stack(lhs_index), Loc::Register(rhs_reg)) => {
            let mut opcodes = vec![match rhs_reg.is_ext() {
                true  => 0x4c,
                false => 0x48,
            }];
            opcodes.push(0x8b);

            let lhs_offset = lhs_index * 8;
            if is_8_bounded(lhs_offset) {
                opcodes.push(0x45 + (rhs_reg.opcode_id() << 3));
                opcodes.push(lhs_offset as u8);
            } else {
                opcodes.push(0x85 + (rhs_reg.opcode_id() << 3));
                append_le32_bytes(&mut opcodes, lhs_offset);
            }
            opcodes
        },
        // FIXME: This can probably be cleaned up a bit.
        //        It shares most code with the above match case.
        (Loc::Register(lhs_reg), Loc::Stack(rhs_index)) => {
            let mut opcodes = vec![match lhs_reg.is_ext() {
                true  => 0x4c,
                false => 0x48,
            }];
            opcodes.push(0x89);

            let rhs_offset = rhs_index * 8;
            if is_8_bounded(rhs_offset) {
                opcodes.push(0x45 + (lhs_reg.opcode_id() << 3));
                opcodes.push(rhs_offset as u8);
            } else {
                opcodes.push(0x85 + (lhs_reg.opcode_id() << 3));
                append_le32_bytes(&mut opcodes, rhs_offset);
            }
            opcodes
        },
        (_, Loc::Immediate(_)) => panic!("Can't move into an immediate"),
        (rhs, lhs) if lhs.is_mem() && rhs.is_mem() =>
            panic!("Can't move memory into memory"),
        (Loc::Indirect(lhs_reg), Loc::Register(rhs_reg)) => {
            let mut opcodes = vec![match (lhs_reg.is_ext(), rhs_reg.is_ext()) {
                (true, true) => 0x4d,
                (true, false) => 0x49,
                (false, true) => 0x4c,
                (false, false) => 0x48,
            }];
            opcodes.push(0x8b);
            opcodes.push(lhs_reg.opcode_id() + (rhs_reg.opcode_id() << 3));
            opcodes
        },
        (Loc::Register(lhs_reg), Loc::Indirect(rhs_reg)) => {
            // TODO: Clean this up. I think I wrote the same match in many places
            let mut opcodes = vec![match (lhs_reg.is_ext(), rhs_reg.is_ext()) {
                (true, true) => 0x4d,
                (true, false) => 0x49,
                (false, true) => 0x4c,
                (false, false) => 0x48,
            }];
            opcodes.push(0x89);
            opcodes.push((lhs_reg.opcode_id() << 3) + rhs_reg.opcode_id());
            opcodes
        },
        (Loc::Data(name), Loc::Register(rhs_reg)) => {
            let vaddr = c.data_vaddrs.get(name).unwrap();
            // Move from absolute address into the register, easy path
            let mut opcodes = vec![if rhs_reg.is_ext() { 0x4c } else { 0x48 }];
            opcodes.push(0x8b);
            opcodes.push(0x04 + (rhs_reg.opcode_id() << 3));
            opcodes.push(0x25);
            append_le32_bytes(&mut opcodes, *vaddr);
            opcodes
        },
        (Loc::Register(lhs_reg), Loc::Data(name)) => {
            let vaddr = c.data_vaddrs.get(name).unwrap();
            let mut opcodes = vec![if lhs_reg.is_ext() { 0x4c } else { 0x48 }];
            opcodes.push(0x89);
            opcodes.push(0x04 + (lhs_reg.opcode_id() << 3));
            opcodes.push(0x25);
            append_le32_bytes(&mut opcodes, *vaddr);
            opcodes
        },
        _ => unimplemented!()
    }
}

fn opcode_gen_op_mul(
    c: &FunContext,
    loc: &Loc,
) -> Inst {
    match loc {
        Loc::Stack(index) => {
            let mut opcodes = vec![0x48, 0xf7];
            let offset = index * 8;

            if is_8_bounded(offset) {
                opcodes.push(0x6d);
                opcodes.push(offset as u8);
            } else {
                opcodes.push(0xad);
                append_le32_bytes(&mut opcodes, offset);
            }
            opcodes
        },
        Loc::Register(reg) => {
            let mut opcodes = vec![match reg.is_ext() {
                true  => 0x49,
                false => 0x48,
            }];
            opcodes.push(0xf7);
            opcodes.push(0xe8 + reg.opcode_id());
            opcodes
        },
        Loc::Data(name) => {
            let vaddr = *c.data_vaddrs.get(name).unwrap();
            let mut opcodes = vec![0xf7, 0x2c, 0x25];
            append_le32_bytes(&mut opcodes, vaddr);
            opcodes
        },
        Loc::Immediate(_) => unreachable!(),
        Loc::Indirect(_) => todo!(),
    }
}

fn opcode_gen_op_div(
    c: &FunContext,
    loc: &Loc,
) -> Inst {
    match loc {
        Loc::Stack(index) => {
            let mut opcodes = vec![0x48, 0xf7];
            let offset = index * 8;

            if is_8_bounded(offset) {
                opcodes.push(0x7d);
                opcodes.push(offset as u8);
            } else {
                opcodes.push(0xbd);
                append_le32_bytes(&mut opcodes, offset);
            }
            opcodes
        },
        Loc::Register(reg) => {
            let mut opcodes = vec![match reg.is_ext() {
                true  => 0x49,
                false => 0x48,
            }];
            opcodes.push(0xf7);
            opcodes.push(0xf8 + reg.opcode_id());
            opcodes
        },
        Loc::Data(name) => {
            let vaddr = *c.data_vaddrs.get(name).unwrap();
            let mut opcodes = vec![0xf7, 0x3c, 0x25];
            append_le32_bytes(&mut opcodes, vaddr);
            opcodes
        },
        Loc::Immediate(_) => unreachable!(),
        Loc::Indirect(_) => todo!(),
    }
}

// Allocates the necessary args on the stack
fn alloc_args(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    pos: &Pos,
    args: &[String],
) -> Result<(), CompErr> {
    for (i, arg) in args.iter().enumerate() {
        let loc = if i < 6 {
            let register = Reg::for_arg_num(i);
            instructions.push(opcode_gen_push(&Loc::Register(register)));
            Loc::Stack(-(i as i64) - 1)
        } else {
            Loc::Stack((i as i64) - 4)
        };

        c.local_var_locs.insert(arg.clone(), loc.clone());
        c.add_to_scope(pos, arg.clone(), ScopeEntry::Var(loc))?;
    }
    Ok(())
}

fn opcode_gen_op_cmp(
    op: &BinOp,
    reg: Reg
) -> Inst {
    let mut opcodes = vec![];
    match reg {
        // The set* opcodes use RAX mode.
        // However, these 4 registers don't need to engage it for some reason.
        Reg::Rax | Reg::Rbx | Reg::Rcx | Reg::Rdx => {},
        reg if reg.is_ext() => {
            opcodes.push(0x41);
        },
        _ => opcodes.push(0x40)
    }
    opcodes.push(0x0f);
    opcodes.push(match op {
        BinOp::Eq => 0x94,
        BinOp::Ne => 0x95,
        BinOp::Le => 0x9e,
        BinOp::Lt => 0x9c,
        BinOp::Ge => 0x9d,
        BinOp::Gt => 0x9f,
        _ => panic!("{:?} is not a comparison operator", op),
    });
    opcodes.push(0xc0 + reg.opcode_id());
    opcodes
}

/// Remember: dest,src
fn opcode_gen_arithmetic(
    c: &FunContext,
    op: &BinOp,
    lhs_loc: &Loc,
    rhs_loc: &Loc,
) -> Inst {
    match (lhs_loc, rhs_loc) {
        (Loc::Immediate(lhs_value), Loc::Register(rhs_reg)) => {
            // NOTE: For 32 bit values, rax has a 1-byte-shorter opcode
            // But for simplicity, I didn't use it here
            let mut opcodes = vec![match rhs_reg.is_ext() {
                true  => 0x49,
                false => 0x48,
            }];
            let arithmetic_base = match op {
                BinOp::Add => 0xc0,
                BinOp::Sub => 0xe8,
                BinOp::And => 0xe0,
                BinOp::Or  => 0xc8,
                BinOp::Xor => 0xf0,
                _ => panic!("{:?} is not a basic arithmetic op", op)
            };
            if is_8_bounded(*lhs_value) {
                opcodes.push(0x83);
                opcodes.push(arithmetic_base + rhs_reg.opcode_id());
                opcodes.push(*lhs_value as u8);
            } else {
                opcodes.push(0x81);
                opcodes.push(arithmetic_base + rhs_reg.opcode_id());
                append_le32_bytes(&mut opcodes, *lhs_value);
            }
            opcodes
        },
        (Loc::Register(lhs_reg), Loc::Register(rhs_reg)) => {
            let mut opcodes = vec![match (lhs_reg.is_ext(), rhs_reg.is_ext()) {
                (true, true)   => 0x4d,
                (true, false)  => 0x4c,
                (false, true)  => 0x49,
                (false, false) => 0x48,
            }];
            opcodes.push(match op {
                BinOp::Add => 0x01,
                BinOp::Sub => 0x29,
                BinOp::And => 0x21,
                BinOp::Or  => 0x09,
                BinOp::Xor => 0x31,
                _ => panic!("{:?} is not a basic arithmetic op", op)
            });
            opcodes.push(0xc0 + (lhs_reg.opcode_id() << 3) + rhs_reg.opcode_id());
            opcodes
        },
        (Loc::Stack(lhs_index), Loc::Register(rhs_reg)) => {
            let mut opcodes = vec![if rhs_reg.is_ext() { 0x4c } else { 0x48 }];
            opcodes.push(match op {
                BinOp::Add => 0x03,
                BinOp::Sub => 0x2b,
                BinOp::And => 0x23,
                BinOp::Or  => 0x0b,
                BinOp::Xor => 0x33,
                _ => panic!("{:?} is not a basic arithmetic op", op)
            });
            let lhs_offset = lhs_index * 8;
            if is_8_bounded(lhs_offset) {
                opcodes.push(0x45 + (rhs_reg.opcode_id() << 3));
                opcodes.push(lhs_offset as u8);
            } else {
                opcodes.push(0x85 + (rhs_reg.opcode_id() << 3));
                append_le32_bytes(&mut opcodes, lhs_offset);
            }
            opcodes
        },
        (Loc::Data(name), Loc::Register(rhs_reg)) => {
            let vaddr = c.data_vaddrs.get(name).unwrap();
            let mut opcodes = vec![if rhs_reg.is_ext() { 0x4c } else { 0x48 }];
            opcodes.push(match op {
                BinOp::Add => 0x03,
                BinOp::Sub => 0x2b,
                BinOp::And => 0x23,
                BinOp::Or  => 0x0b,
                BinOp::Xor => 0x33,
                _ => panic!("{:?} is not a basic arithmetic op", op)
            });
            opcodes.push(0x04 + (rhs_reg.opcode_id() << 3));
            opcodes.push(0x25);
            append_le32_bytes(&mut opcodes, *vaddr);
            opcodes
        },
        _ => unimplemented!()
    }
}

fn opcode_gen_immediate_shift(
    is_left: bool,
    loc: &Loc,
    immediate_value: i64,
) -> Inst {
    match loc {
        Loc::Immediate(_) => panic!("Cannot shift into a literal"),
        Loc::Register(reg) => {
            let mut opcodes = vec![if reg.is_ext() { 0x49 } else { 0x48 }];
            opcodes.push(0xc1);
            let sh_opcode = if is_left { 0xe0 } else { 0xe8 };
            opcodes.push(sh_opcode + reg.opcode_id());
            opcodes.push(immediate_value as u8);
            opcodes
        },
        _ => unimplemented!()
    }
}

fn opcode_gen_cl_shift(
    is_left: bool,
    loc: &Loc
) -> Inst {
    match loc {
        Loc::Register(reg) => {
            let mut opcodes = vec![if reg.is_ext() { 0x49 } else { 0x48 }];
            opcodes.push(0xd3);
            let sh_opcode = if is_left { 0xe0 } else { 0xe8 };
            opcodes.push(sh_opcode + reg.opcode_id());
            opcodes
        },
        _ => unimplemented!()
    }
}

fn opcode_gen_incdec(
    c: &FunContext,
    is_inc: bool,
    loc: &Loc
) -> Inst {
    match loc {
        Loc::Register(reg) => {
            let mut opcodes = vec![if reg.is_ext() { 0x49 } else { 0x48 }];
            opcodes.push(0xff);
            let cmd_opcode = if is_inc { 0xc0 } else { 0xc8 };
            opcodes.push(cmd_opcode + reg.opcode_id());
            opcodes
        },
        Loc::Stack(stack_index) => {
            let stack_offset = stack_index * 8;
            let mut opcodes = vec![0x48, 0xff];
            opcodes.push(match (is_8_bounded(stack_offset), is_inc) {
                (true, true) => 0x45,
                (true, false) => 0x4d,
                (false, true) => 0x85,
                (false, false) => 0x8d,
            });
            if is_8_bounded(stack_offset) {
                opcodes.push(stack_offset as u8);
            } else {
                append_le64_bytes(&mut opcodes, stack_offset);
            }
            opcodes
        },
        Loc::Data(name) => {
            let vaddr = c.data_vaddrs.get(name).unwrap();
            let mut opcodes = vec![0xff];
            opcodes.push(if is_inc { 0x04 } else { 0x0c });
            opcodes.push(0x25);
            append_le32_bytes(&mut opcodes, *vaddr);
            opcodes
        },
        _ => unimplemented!(),
    }
}

fn opcode_gen_not(
    reg: Reg
) -> Inst {
    let mut opcodes = vec![if reg.is_ext () { 0x49 } else { 0x48 }];
    opcodes.push(0xf7);
    opcodes.push(0xd0 + reg.opcode_id());
    opcodes
}

fn opcode_gen_neg(
    reg: Reg
) -> Inst {
    let mut opcodes = vec![if reg.is_ext () { 0x49 } else { 0x48 }];
    opcodes.push(0xf7);
    opcodes.push(0xd8 + reg.opcode_id());
    opcodes
}

fn gen_op_cmp(
    c: &FunContext,
    instructions: &mut Vec<Inst>,
    op: &BinOp,
    lhs_loc: Loc,
    rhs_loc: Loc,
) -> (Loc, RegSet) {
    instructions.push(opcode_gen_cmp(c, &rhs_loc, &lhs_loc));
    instructions.push(opcode_gen_mov(c, &Loc::Immediate(0), &lhs_loc));

    if let Loc::Register(lhs_reg) = lhs_loc {
        instructions.push(opcode_gen_op_cmp(op, lhs_reg));
    } else {
        panic!("LHS must be a register");
    }

    (lhs_loc, RegSet::empty())
}

fn gen_op_single(
    c: &FunContext,
    instructions: &mut Vec<Inst>,
    op: &BinOp,
    lhs_loc: Loc,
    rhs_loc: Loc,
) -> (Loc, RegSet) {
    instructions.push(opcode_gen_arithmetic(c, op, &rhs_loc, &lhs_loc));
    (lhs_loc, RegSet::empty())
}

fn gen_op_shift(
    c: &FunContext,
    instructions: &mut Vec<Inst>,
    is_left: bool,
    lhs_loc: Loc,
    rhs_loc: Loc,
) -> (Loc, RegSet) {
    match rhs_loc {
        Loc::Immediate(imm_value) => {
            instructions.push(opcode_gen_immediate_shift(is_left, &lhs_loc, imm_value));
            (lhs_loc, RegSet::empty())
        },
        _ => {
            // Required to use %cl register for non immediates during shifts
            instructions.push(opcode_gen_mov(c, &rhs_loc, &Reg::Rcx.into()));
            instructions.push(opcode_gen_cl_shift(is_left, &lhs_loc));
            let used_registers = RegSet::of(Reg::Rcx);
            (lhs_loc, used_registers)
        }
    }
}

/**
 * Prepares sending an LHS and RHS to the FPU
 * @return (instructions, rhs_loc, used_registers)
 */
fn gen_op_pre_float_op(
    c: &FunContext,
    instructions: &mut Vec<Inst>,
    lhs_loc: Loc,
    init_rhs_loc: Loc,
) -> (Loc, RegSet) {
    // rax and rdx are always used for div or mod
    let mut used_registers = RegSet::of(Reg::Rax).with(Reg::Rdx);

    let should_move_rhs = matches!(
        init_rhs_loc,
        Loc::Register(Reg::Rax) | Loc::Register(Reg::Rdx) | Loc::Immediate(_)
    );
    // Move rhs to a new register if necessary
    let rhs_loc = if should_move_rhs {
        // Make sure we don't override LHS
        let dest_reg = match lhs_loc {
            Loc::Register(Reg::Rcx) => Reg::Rdi,
            _ => Reg::Rcx,
        };
        used_registers = used_registers.with(dest_reg);
        instructions.push(opcode_gen_mov(c, &init_rhs_loc, &dest_reg.into()));
        Loc::Register(dest_reg)
    } else {
        init_rhs_loc
    };
    match lhs_loc {
        Loc::Register(Reg::Rax) => {}
        lhs_loc => instructions.push(opcode_gen_mov(c, &lhs_loc, &Reg::Rax.into())),
    }
    (rhs_loc, used_registers)
}

/**
 * Generate instructions to diving (or mod) two numbers
 * As per x86 idivq, the results always go to rax and rdx
 */
fn gen_op_pre_div(
    c: &FunContext,
    instructions: &mut Vec<Inst>,
    lhs_loc: Loc,
    init_rhs_loc: Loc
) -> RegSet {
    let (rhs_loc, used_registers) = gen_op_pre_float_op(c, instructions, lhs_loc, init_rhs_loc);
    instructions.push(opcode_gen_mov(c, &0.into(), &Reg::Rdx.into()));
    instructions.push(opcode_gen_op_div(c, &rhs_loc));
    used_registers
}

fn gen_op_mod(
    c: &FunContext,
    instructions: &mut Vec<Inst>,
    lhs_loc: Loc,
    rhs_loc: Loc
) -> (Loc, RegSet) {
    let used_registers = gen_op_pre_div(c, instructions, lhs_loc, rhs_loc);
    (Loc::Register(Reg::Rdx), used_registers)
}

fn gen_op_div(
    c: &FunContext,
    instructions: &mut Vec<Inst>,
    lhs_loc: Loc,
    rhs_loc: Loc
) -> (Loc, RegSet) {
    let used_registers = gen_op_pre_div(c, instructions, lhs_loc, rhs_loc);
    (Loc::Register(Reg::Rax), used_registers)
}

fn gen_op_mul(
    c: &FunContext,
    instructions: &mut Vec<Inst>,
    lhs_loc: Loc,
    rhs_loc: Loc
) -> (Loc, RegSet) {
    let (rhs_loc, used_registers) = gen_op_pre_float_op(c, instructions, lhs_loc, rhs_loc);
    instructions.push(opcode_gen_op_mul(c, &rhs_loc));
    (Loc::Register(Reg::Rax), used_registers)
}

/**
 * Generates instructions for applying the operator to the given lhs & rhs.
 * May override either lhs_loc or rhs_loc if they are registers.
 * ASSUMES lhs_loc is already in a register
 * @return (instructions, dest_loc, extra_registers)
 */
fn gen_op_command(
    c: &FunContext,
    instructions: &mut Vec<Inst>,
    op: &BinOp,
    lhs_loc: Loc,
    rhs_loc: Loc,
) -> (Loc, RegSet) {
    match op {
        BinOp::Add => gen_op_single(c, instructions, op, lhs_loc, rhs_loc),
        BinOp::Sub => gen_op_single(c, instructions, op, lhs_loc, rhs_loc),
        BinOp::Mod => gen_op_mod(c, instructions, lhs_loc, rhs_loc),
        BinOp::Div => gen_op_div(c, instructions, lhs_loc, rhs_loc),
        BinOp::Mul => gen_op_mul(c, instructions, lhs_loc, rhs_loc),
        BinOp::ShiftRight => gen_op_shift(c, instructions, false, lhs_loc, rhs_loc),
        BinOp::ShiftLeft => gen_op_shift(c, instructions, true, lhs_loc, rhs_loc),
        BinOp::And => gen_op_single(c, instructions, op, lhs_loc, rhs_loc),
        BinOp::Or => gen_op_single(c, instructions, op, lhs_loc, rhs_loc),
        BinOp::Xor => gen_op_single(c, instructions, op, lhs_loc, rhs_loc),
        BinOp::Eq => gen_op_cmp(c, instructions, op, lhs_loc, rhs_loc),
        BinOp::Ne => gen_op_cmp(c, instructions, op, lhs_loc, rhs_loc),
        BinOp::Le => gen_op_cmp(c, instructions, op, lhs_loc, rhs_loc),
        BinOp::Lt => gen_op_cmp(c, instructions, op, lhs_loc, rhs_loc),
        BinOp::Ge => gen_op_cmp(c, instructions, op, lhs_loc, rhs_loc),
        BinOp::Gt => gen_op_cmp(c, instructions, op, lhs_loc, rhs_loc),
        BinOp::Assign(_) => panic!("Assignments should not be parsed as regular binop exprs"),
    }
}

/// Returns the registers that the given op MIGHT use
fn registers_for_op(op: &BinOp) -> RegSet {
    let fpu_regset = RegSet::of(Reg::Rax).with(Reg::Rdx);
    let shift_regset = RegSet::of(Reg::Rcx);

    match op {
        BinOp::ShiftRight => shift_regset,
        BinOp::ShiftLeft => shift_regset,
        BinOp::Mod => fpu_regset,
        BinOp::Div => fpu_regset,
        BinOp::Mul => fpu_regset,
        _ => RegSet::empty(),
    }
}

fn get_safe_registers(used_registers: RegSet) -> RegSet {
    RegSet::usable_caller_save().subtract(&used_registers)
}

/// Plans evaluation strategy for an LHS and RHS expression.
/// It will try to store the LHS value in a register if possible.
/// Otherwise it stores to the stack.
/// The returned `lhs_loc` is guaranteed to be in a register
/// Return format: `(instructions, lhs_loc, rhs_loc, used_registers)`
/// #Arguments
/// * `unsafe_registers` - Registers which should be avoided
fn gen_pre_op(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    lhs: &Expr,
    rhs: &Expr,
    unsafe_registers: RegSet,
) -> Result<(Loc, Loc, RegSet), CompErr> {
    // Generate instructions for RHS first so we know which registers are safe
    let mut rhs_ins = vec![];
    let (rhs_loc, mut used_registers) = gen_expr(c, &mut rhs_ins, rhs)?;

    let safe_registers = get_safe_registers(used_registers.clone()).subtract(&unsafe_registers);

    let (lhs_loc, lhs_registers) = gen_expr(c, instructions, lhs)?;
    used_registers = used_registers.union(lhs_registers);

    // If the lhs_loc is already in a safe register, don't move it
    if let Loc::Register(lhs_reg) = lhs_loc {
        if safe_registers.contains(lhs_reg) {
            instructions.append(&mut rhs_ins);
            return Ok((lhs_loc, rhs_loc, used_registers));
        }
    }

    let new_lhs_loc = match safe_registers.first() {
        // If there are safe registers available, store the lhs there
        Some(dest_reg) => {
            used_registers = used_registers.with(dest_reg);
            instructions.push(opcode_gen_mov(c, &lhs_loc, &dest_reg.into()));
            instructions.append(&mut rhs_ins);
            Loc::Register(dest_reg)
        }
        // Nowhere is safe! Store LHS on the stack
        None => {
            let lhs_in_reg = matches!(lhs_loc, Loc::Register(_));

            if lhs_in_reg {
                instructions.push(opcode_gen_push(&lhs_loc));
            }

            instructions.append(&mut rhs_ins);

            // Don't need to update used_registers because...
            // we already know everything is used!
            let new_lhs_loc = match rhs_loc {
                Loc::Register(Reg::Rax) => Loc::Register(Reg::Rcx),
                _ => Loc::Register(Reg::Rax),
            };

            if lhs_in_reg {
                instructions.push(opcode_gen_pop(&new_lhs_loc));
            } else {
                instructions.push(opcode_gen_mov(c, &lhs_loc, &new_lhs_loc));
            }
            new_lhs_loc
        }
    };

    Ok((new_lhs_loc, rhs_loc, used_registers))
}

fn gen_prep_unary_op_incdec(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (expr_loc, used_registers) = gen_expr(c, instructions, expr)?;

    match expr_loc {
        Loc::Register(_) | Loc::Immediate(_) => {
            return CompErr::err(
                &expr.pos(),
                "`++` or `--` must operate on a memory location".to_string(),
            )
        }
        _ => {}
    };
    Ok((expr_loc, used_registers))
}

fn gen_unary_op_pre_incdec(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    is_inc: bool,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (expr_loc, used_registers) = gen_prep_unary_op_incdec(c, instructions, expr)?;
    instructions.push(opcode_gen_incdec(c, is_inc, &expr_loc));
    Ok((expr_loc, used_registers))
}

fn gen_unary_op_post_incdec(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    is_inc: bool,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (expr_loc, used_registers) = gen_prep_unary_op_incdec(c, instructions, expr)?;

    // We can pick any register since we know expr_loc MUST NOT be in a register
    let dest_reg = Reg::R11;
    let dest_loc = Loc::Register(dest_reg);

    instructions.push(opcode_gen_mov(c, &expr_loc, &dest_loc));
    instructions.push(opcode_gen_incdec(c, is_inc, &expr_loc));
    Ok((dest_loc, used_registers.with(dest_reg)))
}

/// Returns the register to perform the operation on (the dest op)
fn gen_unary_pre_op(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    expr: &Expr,
) -> Result<(Reg, RegSet), CompErr> {
    let (expr_loc, mut used_registers) = gen_expr(c, instructions, expr)?;
    let dest_reg = match expr_loc {
        Loc::Register(reg) => reg,
        _ => {
            let dest_reg = match used_registers.first() {
                Some(reg) => reg,
                None => {
                    used_registers = used_registers.with(Reg::Rax);
                    Reg::Rax
                }
            };
            instructions.push(opcode_gen_mov(c, &expr_loc, &dest_reg.into()));
            dest_reg
        }
    };
    Ok((dest_reg, used_registers))
}

fn gen_unary_op_not(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (dest_reg, used_registers) = gen_unary_pre_op(c, instructions, expr)?;
    instructions.push(opcode_gen_not(dest_reg));
    Ok((Loc::Register(dest_reg), used_registers))
}

fn gen_unary_op_neg(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (dest_reg, used_registers) = gen_unary_pre_op(c, instructions, expr)?;
    instructions.push(opcode_gen_neg(dest_reg));
    Ok((Loc::Register(dest_reg), used_registers))
}

fn gen_unary_op(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    op: &UnaryOp,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    match op {
        UnaryOp::PreIncrement => gen_unary_op_pre_incdec(c, instructions, true, expr),
        UnaryOp::PreDecrement => gen_unary_op_pre_incdec(c, instructions, false, expr),
        UnaryOp::PostIncrement => gen_unary_op_post_incdec(c, instructions, true, expr),
        UnaryOp::PostDecrement => gen_unary_op_post_incdec(c, instructions, false, expr),
        UnaryOp::BitNot => gen_unary_op_not(c, instructions, expr),
        UnaryOp::Negate => gen_unary_op_neg(c, instructions, expr),
    }
}

fn gen_bin_op(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    op: &BinOp,
    lhs: &Expr,
    rhs: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (lhs_loc, rhs_loc, used_registers) =
        gen_pre_op(c, instructions, lhs, rhs, registers_for_op(op))?;
    // Run the command!
    let (op_loc, op_registers) = gen_op_command(c, instructions, op, lhs_loc, rhs_loc);
    Ok((op_loc, used_registers.union(op_registers)))
}

fn gen_syscall(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    pos: &Pos,
    params: &Vec<Expr>,
) -> Result<(Loc, RegSet), CompErr> {
    if params.is_empty() || params.len() > 7 {
        return CompErr::err(pos, "syscall() must take between 1-7 arguments".to_string());
    }

    let mut used_registers = RegSet::of(Reg::Rax);

    for param in params.iter().rev() {
        let (param_loc, param_used_reg) = gen_expr(c, instructions, param)?;

        // TODO: Optimize better, quit monkeying around
        // No point pushing memory or immediate locations to the stack!
        instructions.push(opcode_gen_push(&param_loc));

        used_registers = used_registers.union(param_used_reg);
    }

    for i in 0..params.len() {
        let reg = Reg::for_syscall_arg_num(i);
        instructions.push(opcode_gen_pop(&Loc::Register(reg)));
    }
    // Syscall opcode
    instructions.push(vec![0x0f, 0x05]);
    Ok((Loc::Register(Reg::Rax), used_registers))
}

// Returns the first 6 param locations
fn gen_call_params(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    params: &Vec<Expr>,
) -> Result<Vec<Loc>, CompErr> {
    // Evaluate backwards until the 7th var.
    // Since the 7th+ params have to be on the stack anyways
    for i in (6..params.len()).rev() {
        let param = &params[i];
        let (param_loc, _) = gen_expr(c, instructions, param)?;
        instructions.push(opcode_gen_push(&param_loc));
    }

    let mut param_locs = vec![];

    for param in params.iter().take(std::cmp::min(6, params.len())) {
        let (param_loc, _) = gen_expr(c, instructions, param)?;

        if param_loc.is_reg() {
            instructions.push(opcode_gen_push(&param_loc));
        }
        param_locs.push(param_loc);
    }

    Ok(param_locs)
}

/// Substitutes the given IDs in the AST to their respective values.
/// Used for macro expansion
fn substitute_id(body: &Expr, substitutions: &HashMap<String, Expr>) -> Result<Expr, CompErr> {
    Ok(match body {
        expr @ Expr::Str(_, _) => expr.clone(),
        expr @ Expr::Int(_, _) => expr.clone(),
        Expr::Id(pos, name) => match substitutions.get(name) {
            Some(value) => value.clone(),
            None => Expr::Id(pos.clone(), name.clone()),
        },
        Expr::Reference(pos, name) => match substitutions.get(name) {
            Some(_) => return CompErr::err(pos, "Cannot reference a macro arg".to_string()),
            None => Expr::Reference(pos.clone(), name.clone()),
        },
        Expr::Call(pos, callee, params) => {
            let sub_callee = substitute_id(callee, substitutions)?;
            let mut sub_params = vec![];
            for param in params {
                sub_params.push(substitute_id(param, substitutions)?);
            }
            Expr::Call(pos.clone(), Box::new(sub_callee), sub_params)
        }
        Expr::Assignment(pos, lhs, rhs) => {
            let sub_rhs = substitute_id(rhs, substitutions)?;
            Expr::Assignment(pos.clone(), lhs.clone(), Box::new(sub_rhs))
        }
        Expr::DerefAssignment(pos, lhs, rhs) => {
            let sub_lhs = substitute_id(lhs, substitutions)?;
            let sub_rhs = substitute_id(rhs, substitutions)?;
            Expr::DerefAssignment(pos.clone(), Box::new(sub_lhs), Box::new(sub_rhs))
        }
        Expr::UnaryOperator(pos, op, expr) => {
            let sub_expr = substitute_id(expr, substitutions)?;
            Expr::UnaryOperator(pos.clone(), op.clone(), Box::new(sub_expr))
        }
        Expr::BinOperator(pos, op, lhs, rhs) => {
            let sub_lhs = substitute_id(lhs, substitutions)?;
            let sub_rhs = substitute_id(rhs, substitutions)?;
            Expr::BinOperator(
                pos.clone(),
                op.clone(),
                Box::new(sub_lhs),
                Box::new(sub_rhs),
            )
        }
        Expr::Cond(pos, cond, truthy, falsey) => {
            let sub_cond = substitute_id(cond, substitutions)?;
            let sub_truthy = substitute_id(truthy, substitutions)?;
            let sub_falsey = substitute_id(falsey, substitutions)?;
            Expr::Cond(
                pos.clone(),
                Box::new(sub_cond),
                Box::new(sub_truthy),
                Box::new(sub_falsey),
            )
        }
        Expr::Dereference(pos, expr) => {
            let sub_expr = substitute_id(expr, substitutions)?;
            Expr::Dereference(pos.clone(), Box::new(sub_expr))
        }
    })
}

fn gen_call_macro(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    pos: &Pos,
    body: Expr,
    args: Vec<String>,
    params: &Vec<Expr>,
) -> Result<(Loc, RegSet), CompErr> {
    if args.len() != params.len() {
        return CompErr::err(
            pos,
            format!("This macro must accept {} arguments", args.len()),
        );
    }

    let mut substitutions = HashMap::new();
    for i in 0..args.len() {
        substitutions.insert(args[i].clone(), params[i].clone());
    }
    let sub_body = substitute_id(&body, &substitutions)?;
    gen_expr(c, instructions, &sub_body)
}

fn gen_call(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    pos: &Pos,
    callee_expr: &Expr,
    params: &Vec<Expr>,
) -> Result<(Loc, RegSet), CompErr> {
    match callee_expr {
        Expr::Id(_, name) if name == "syscall" => {
            return gen_syscall(c, instructions, pos, params);
        }
        _ => {}
    }
    let param_locs = gen_call_params(c, instructions, params)?;

    let callee = match callee_expr {
        Expr::Id(_, name) => match c.find_in_scope(name) {
            Some(ScopeEntry::Fun(arg_num)) => {
                if params.len() > *arg_num {
                    return CompErr::err(
                        pos,
                        format!("{} accepts at most {} arguments", name, arg_num),
                    );
                }
                name
            }
            Some(ScopeEntry::Var(loc)) => {
                instructions.push(opcode_gen_mov(c, loc, &Reg::Rax.into()));
                "*%rax"
            }
            Some(ScopeEntry::Define(args, body)) => {
                let ac = args.clone();
                let bc = body.clone();
                let pc = pos.clone();
                return gen_call_macro(c, instructions, &pc, bc, ac, params);
            }
            None => return CompErr::err(pos, format!("{} not in scope", name)),
        },
        callee_expr => {
            let (callee_loc, _) = gen_expr(c, instructions, callee_expr)?;
            if callee_loc != Loc::Register(Reg::Rax) {
                instructions.push(opcode_gen_mov(c, &callee_loc, &Reg::Rax.into()));
            }
            "*%rax"
        }
    };
    for i in (0..std::cmp::min(6, params.len())).rev() {
        let reg = Reg::for_arg_num(i);
        let param_loc = &param_locs[i];

        if param_loc.is_reg() {
            instructions.push(opcode_gen_pop(&Loc::Register(reg)));
        } else {
            instructions.push(opcode_gen_mov(c, param_loc, &reg.into()));
        }
    }
    // The call offset is calculated after all the functions are done generating
    if callee == "*%rax" {
        instructions.push(vec![0xff, 0xd0]);
    } else {
        c.inst_links.push((instructions.len(), callee.to_string()));
        instructions.push(vec![0xe8, 0, 0, 0, 0]);
    }
    if params.len() > 6 {
        let stack_arg_count = params.len() - 6;
        let stack_arg_imm = 8 * stack_arg_count as i64;
        instructions.push(opcode_gen_arithmetic(c, &BinOp::Add, &stack_arg_imm.into(), &Reg::Rsp.into()));
    }
    // Assume we used all the registers, since we're calling an unknown function
    Ok((Loc::Register(Reg::Rax), RegSet::usable_caller_save()))
}

fn opcode_gen_lea(
    src_loc: &Loc,
    dst_reg: Reg,
) -> Inst {
    match src_loc {
        Loc::Immediate(_) => panic!("No address for immediates"),
        Loc::Register(_) => panic!("No address for registers"),
        Loc::Stack(stack_index) => {
            let mut opcodes = vec![if dst_reg.is_ext() { 0x4c } else { 0x48 }];
            opcodes.push(0x8d);
            let stack_offset = stack_index * 8;
            if is_8_bounded(stack_offset) {
                opcodes.push(0x45 + (dst_reg.opcode_id() << 3));
                opcodes.push(stack_offset as u8);
            } else {
                opcodes.push(0x85 + (dst_reg.opcode_id() << 3));
                append_le32_bytes(&mut opcodes, stack_offset);
            }
            opcodes
        },
        _ => unimplemented!()
    }
}

fn is_jump(inst: &Inst) -> bool {
    if inst[0] == 0xe9 {
        true
    } else if inst[0] == 0x0f {
        matches!(inst[1], 0x85 | 0x84 | 0x8e | 0x8c | 0x8d | 0x8f)
    } else {
        false
    }
}

// NOTE: The target jump location is actually the index in label_indices
// The proper relative jump offset is added at the end of gen_fun
fn opcode_gen_jump(
    jump: Jump,
    label: usize
) -> Inst {
    // Special snowflake
    if jump == Jump::Jmp {
        let mut opcodes = vec![0xe9];
        append_le32_bytes(&mut opcodes, label as i64);
        return opcodes;
    }
    let mut opcodes = vec![0x0f, match jump {
        Jump::Jmp => unreachable!(),
        Jump::Jne => 0x85,
        Jump::Je  => 0x84,
        Jump::Jle => 0x8e,
        Jump::Jl  => 0x8c,
        Jump::Jge => 0x8d,
        Jump::Jg  => 0x8f,
    }];
    append_le32_bytes(&mut opcodes, label as i64);
    opcodes
}

fn gen_reference(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    pos: &Pos,
    name: &String,
) -> Result<(Loc, RegSet), CompErr> {
    match c.find_in_scope(name) {
        Some(ScopeEntry::Var(src_loc @ Loc::Stack(_))) => {
            let dest_reg = Reg::Rax;
            instructions.push(opcode_gen_lea(src_loc, dest_reg));
            Ok((dest_reg.into(), RegSet::of(dest_reg)))
        }
        Some(ScopeEntry::Var(other)) => {
            CompErr::err(pos, format!("Variable cannot be at {:?}!", other))
        }
        Some(ScopeEntry::Fun(_)) => {
            let dest_reg = Reg::Rax;
            instructions.push(vec![]);
            Ok((Loc::Register(dest_reg), RegSet::of(dest_reg)))
        }
        Some(ScopeEntry::Define(_, _)) => {
            CompErr::err(pos, "#define value cannot be referenced".to_string())
        }
        None => CompErr::err(pos, format!("{} not in scope", name)),
    }
}

fn gen_dereference(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (target_loc, mut used_registers) = gen_expr(c, instructions, expr)?;

    let dest_reg = match target_loc {
        // Reuse the target register
        Loc::Register(dest_reg) => dest_reg,
        _ => {
            let new_reg = match used_registers.first() {
                Some(reg) => reg,
                None => {
                    used_registers = used_registers.with(Reg::Rax);
                    Reg::Rax
                }
            };
            instructions.push(opcode_gen_mov(c, &target_loc, &new_reg.into()));
            new_reg
        }
    };
    instructions.push(opcode_gen_mov(c, &Loc::Indirect(dest_reg), &dest_reg.into()));
    Ok((Loc::Register(dest_reg), used_registers))
}

// Generates the RHS instructions for an assignment
fn gen_expr_ass_rhs(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    rhs: &Expr,
) -> Result<(Reg, RegSet), CompErr> {
    let (rhs_loc, mut used_registers) = gen_expr(c, instructions, rhs)?;

    match rhs_loc {
        Loc::Register(reg) => Ok((reg, used_registers)),
        // Because we can't move from memory to memory
        rhs_loc => {
            // Reuse a register if possible
            let rhs_reg = match used_registers.first() {
                Some(reg) => reg,
                None => {
                    used_registers = used_registers.with(Reg::Rax);
                    Reg::Rax
                }
            };

            instructions.push(opcode_gen_mov(c, &rhs_loc, &rhs_reg.into()));
            Ok((rhs_reg, used_registers))
        }
    }
}

fn gen_expr_ass(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    pos: &Pos,
    lhs_name: &String,
    rhs: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (rhs_reg, used_registers) = gen_expr_ass_rhs(c, instructions, rhs)?;

    match c.find_in_scope(lhs_name) {
        Some(ScopeEntry::Var(lhs_loc)) => {
            instructions.push(opcode_gen_mov(c, &rhs_reg.into(), lhs_loc));
            Ok((lhs_loc.clone(), used_registers))
        }
        Some(ScopeEntry::Fun(_)) => CompErr::err(pos, "Cannot reassign a function".to_string()),
        Some(ScopeEntry::Define(_, _)) => {
            CompErr::err(pos, "Cannot reassign a #define value".to_string())
        }
        None => CompErr::err(pos, format!("Variable {} not in scope", lhs_name)),
    }
}

// FIXME: This register assignment technique is super similar in other places
// Abstract away!
fn gen_expr_deref_ass(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    lhs: &Expr,
    rhs: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    let (lhs_loc, mut used_registers) = gen_expr(c, instructions, lhs)?;
    let mut rhs_inst = vec![];
    let (rhs_reg, rhs_used) = gen_expr_ass_rhs(c, &mut rhs_inst, rhs)?;
    let safe_registers = get_safe_registers(rhs_used.clone());

    let lhs_dest_reg = match safe_registers.first() {
        Some(safe_reg) => match lhs_loc {
            Loc::Register(lhs_reg) if safe_registers.contains(lhs_reg) => Some(lhs_reg),
            lhs_loc => {
                instructions.push(opcode_gen_mov(c, &lhs_loc, &safe_reg.into()));
                used_registers = used_registers.with(safe_reg);
                Some(safe_reg)
            }
        },
        None => {
            // No safe registers! Push to stack!
            instructions.push(opcode_gen_push(&lhs_loc));
            None
        }
    };

    instructions.append(&mut rhs_inst);
    used_registers = used_registers.union(rhs_used);

    // If needed, pull the LHS address back into a register
    let dest_reg = match lhs_dest_reg {
        Some(dest_reg) => dest_reg,
        // Pull the LHS address off the stack, into an unused register
        None => {
            let dest_reg = match rhs_reg {
                Reg::Rax => Reg::Rcx,
                _ => Reg::Rax,
            };
            instructions.push(opcode_gen_pop(&Loc::Register(dest_reg)));
            dest_reg
        }
    };

    // At this point
    // - instructions for both LHS and RHS are done
    // - The lhs address is at dest_reg
    // - The rhs value is at rhs_reg
    instructions.push(opcode_gen_mov(c, &rhs_reg.into(), &Loc::Indirect(dest_reg)));
    Ok((Loc::Register(rhs_reg), used_registers))
}

/// # Arguments
/// * `dest_reg` - For values > 2^32, this is the reg that will be used.
fn gen_int(
    c: &FunContext,
    instructions: &mut Vec<Inst>,
    signed: i64,
    dest_reg: Reg
) -> (Loc, RegSet) {
    if is_32_bounded(signed) {
        (Loc::Immediate(signed), RegSet::empty())
    } else {
        let unsigned = signed as u64;
        let upper = ((unsigned & !((1 << 32) - 1)) >> 32) as i64;
        let lower = (unsigned & ((1 << 32) - 1)) as i64;

        // Since immediates can only be 32 bit ints at most
        instructions.push(opcode_gen_mov(c, &upper.into(), &dest_reg.into()));
        instructions.push(opcode_gen_immediate_shift(true, &dest_reg.into(), 32));

        // addq doesn't support large immediate values... I know, this is gnarly
        // But really, such massive ints aren't that common.
        // So suboptimal opcodes are ok I guess
        let tmp_reg = match dest_reg {
            Reg::Rax => Reg::Rcx,
            _        => Reg::Rax,
        };
        instructions.push(opcode_gen_mov(c, &lower.into(), &tmp_reg.into()));
        instructions.push(opcode_gen_arithmetic(c, &BinOp::Add, &tmp_reg.into(), &dest_reg.into()));

        (Loc::Register(dest_reg), RegSet::of(dest_reg).with(tmp_reg))
    }
}

fn gen_cond_expr(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    cond_expr: &Expr,
    true_expr: &Expr,
    false_expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    // Move the results to %rax, kinda arbitrarily
    let dest_reg = Reg::Rax;
    let dest_loc = Loc::Register(dest_reg);
    let mut used_registers = RegSet::of(dest_reg);

    let true_end_label = c.new_label();
    let false_end_label = c.new_label();
    gen_cond(c, instructions, cond_expr, true_end_label)?;

    let (true_loc, true_used) = gen_expr(c, instructions, true_expr)?;
    used_registers = used_registers.union(true_used);
    if true_loc != dest_loc {
        instructions.push(opcode_gen_mov(c, &true_loc, &dest_loc));
    }
    instructions.push(opcode_gen_jump(Jump::Jmp, false_end_label));
    c.label_indices[true_end_label] = instructions.len();

    let (false_loc, false_used) = gen_expr(c, instructions, false_expr)?;
    used_registers = used_registers.union(false_used);
    if false_loc != dest_loc {
        instructions.push(opcode_gen_mov(c, &false_loc, &dest_loc));
    }
    c.label_indices[false_end_label] = instructions.len();

    Ok((dest_loc, used_registers))
}

/**
 * @return (instructions, location, used_registers)
 */
fn gen_expr(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    expr: &Expr,
) -> Result<(Loc, RegSet), CompErr> {
    match expr {
        Expr::Int(_, value) => Ok(gen_int(c, instructions, *value, Reg::Rax)),
        Expr::Id(pos, name) => match c.find_in_scope(name) {
            Some(ScopeEntry::Var(loc)) => Ok((loc.clone(), RegSet::empty())),
            Some(ScopeEntry::Fun(_)) => CompErr::err(
                pos,
                format!(
                    "{} is a function, and can only be called or referenced",
                    name
                ),
            ),
            Some(ScopeEntry::Define(args, body)) => {
                if args.is_empty() {
                    let b = body.clone();
                    gen_expr(c, instructions, &b)
                } else {
                    CompErr::err(pos, format!("This macro must take {} args", args.len()))
                }
            }
            None => CompErr::err(pos, format!("Variable {} not in scope", name)),
        },
        Expr::Str(_, string_id) => {
            let vaddr = c.str_vaddrs.get(string_id).unwrap();
            let opcodes = opcode_gen_mov(c, &Loc::Immediate(*vaddr), &Reg::Rax.into());
            instructions.push(opcodes);
            Ok((Loc::Register(Reg::Rax), RegSet::of(Reg::Rax)))
        },
        Expr::Assignment(pos, lhs, rhs) => gen_expr_ass(c, instructions, pos, lhs, rhs),
        Expr::DerefAssignment(_, lhs, rhs) => gen_expr_deref_ass(c, instructions, lhs, rhs),
        Expr::UnaryOperator(_, op, expr) => gen_unary_op(c, instructions, op, expr),
        Expr::BinOperator(_, op, lhs, rhs) => gen_bin_op(c, instructions, op, lhs, rhs),
        Expr::Call(pos, callee, params) => gen_call(c, instructions, pos, callee, params),
        Expr::Reference(pos, name) => gen_reference(c, instructions, pos, name),
        Expr::Dereference(_, expr) => gen_dereference(c, instructions, expr),
        Expr::Cond(_, cond, true_expr, false_expr) => {
            gen_cond_expr(c, instructions, cond, true_expr, false_expr)
        }
    }
}

fn gen_return_expr(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    expr: &Expr,
) -> Result<(), CompErr> {
    let (loc, _) = gen_expr(c, instructions, expr)?;

    // If the location is already rax, we don't need to move!
    if loc != Loc::Register(Reg::Rax) {
        instructions.push(opcode_gen_mov(c, &loc, &Reg::Rax.into()));
    }
    // leave
    instructions.push(vec![0xc9]);
    // ret
    instructions.push(vec![0xc3]);
    Ok(())
}

fn gen_return(instructions: &mut Vec<Inst>) {
    // xor %rax,%rax, effectively mov $0,%rax
    instructions.push(vec![0x48, 0x31, 0xc0]);
    // leave
    instructions.push(vec![0xc9]);
    // ret
    instructions.push(vec![0xc3]);
}

fn gen_cond_cmp(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    jump: Jump,
    lhs: &Expr,
    rhs: &Expr,
    end_label: usize,
) -> Result<(), CompErr> {
    let (lhs_loc, rhs_loc, _) = gen_pre_op(c, instructions, lhs, rhs, RegSet::empty())?;
    instructions.push(opcode_gen_cmp(c, &rhs_loc, &lhs_loc));
    instructions.push(opcode_gen_jump(jump, end_label));
    Ok(())
}

fn gen_cond(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    cond: &Expr,
    end_label: usize,
) -> Result<(), CompErr> {
    match cond {
        Expr::BinOperator(_, BinOp::Eq, lhs, rhs) => {
            gen_cond_cmp(c, instructions, Jump::Jne, lhs, rhs, end_label)
        }
        Expr::BinOperator(_, BinOp::Ne, lhs, rhs) => {
            gen_cond_cmp(c, instructions, Jump::Je, lhs, rhs, end_label)
        }
        Expr::BinOperator(_, BinOp::Gt, lhs, rhs) => {
            gen_cond_cmp(c, instructions, Jump::Jle, lhs, rhs, end_label)
        }
        Expr::BinOperator(_, BinOp::Ge, lhs, rhs) => {
            gen_cond_cmp(c, instructions, Jump::Jl, lhs, rhs, end_label)
        }
        Expr::BinOperator(_, BinOp::Lt, lhs, rhs) => {
            gen_cond_cmp(c, instructions, Jump::Jge, lhs, rhs, end_label)
        }
        Expr::BinOperator(_, BinOp::Le, lhs, rhs) => {
            gen_cond_cmp(c, instructions, Jump::Jg, lhs, rhs, end_label)
        }
        Expr::Int(_, value) => {
            if *value == 0 {
                instructions.push(opcode_gen_jump(Jump::Jmp, end_label));
                Ok(())
            } else {
                // For non-zero ints, no comparison needs to be made!
                Ok(())
            }
        }
        _ => {
            // Fallback to evaluating the entire conditional expression
            let (cond_loc, _) = gen_expr(c, instructions, cond)?;
            instructions.push(opcode_gen_cmp(c, &0.into(), &cond_loc));
            instructions.push(opcode_gen_jump(Jump::Je, end_label));
            Ok(())
        }
    }
}

fn gen_if(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    cond: &Expr,
    if_body: &Statement,
) -> Result<(), CompErr> {
    let if_end_label = c.new_label();
    gen_cond(c, instructions, cond, if_end_label)?;
    gen_statement(c, instructions, if_body)?;
    c.label_indices[if_end_label] = instructions.len();
    Ok(())
}

fn gen_while(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    cond: &Expr,
    body: &Statement,
) -> Result<(), CompErr> {
    let while_begin_label = c.new_label();
    c.label_indices[while_begin_label] = instructions.len();
    let while_end_label = c.new_label();

    c.break_dest_stack.push(while_end_label);

    gen_cond(c, instructions, cond, while_end_label)?;
    gen_statement(c, instructions, body)?;
    instructions.push(opcode_gen_jump(Jump::Jmp, while_begin_label));
    c.label_indices[while_end_label] = instructions.len();

    c.break_dest_stack.pop();
    Ok(())
}

fn gen_if_else(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    cond: &Expr,
    if_body: &Statement,
    else_body: &Statement,
) -> Result<(), CompErr> {
    let if_end_label = c.new_label();
    let else_end_label = c.new_label();

    gen_cond(c, instructions, cond, if_end_label)?;
    gen_statement(c, instructions, if_body)?;
    instructions.push(opcode_gen_jump(Jump::Jmp, else_end_label));
    c.label_indices[if_end_label] = instructions.len();
    gen_statement(c, instructions, else_body)?;
    c.label_indices[else_end_label] = instructions.len();
    Ok(())
}

fn gen_switch(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    cond: &Expr,
    body: &Vec<SwInner>,
) -> Result<(), CompErr> {
    let (expr_loc, _) = gen_expr(c, instructions, cond)?;
    // cmp requires the dest to be in a register
    let cond_loc = match expr_loc {
        loc @ Loc::Register(_) => loc,
        other => {
            instructions.push(opcode_gen_mov(c, &other, &Reg::Rax.into()));
            Loc::Register(Reg::Rax)
        }
    };

    // The register to store case values
    let case_reg = match cond_loc {
        Loc::Register(Reg::Rax) => Reg::Rcx,
        _ => Reg::Rax,
    };

    let mut used_case_values = HashSet::new();
    let mut default_label: Option<usize> = None;
    let mut body_inst = vec![];

    let switch_end_label = c.new_label();
    // We first store these as offsets indexed at body_inst
    // Then we increment them based on the `instructions`
    let mut case_label_indices = vec![];

    c.break_dest_stack.push(switch_end_label);
    for inner in body {
        match inner {
            SwInner::Default(pos) => {
                if default_label.is_some() {
                    return CompErr::err(
                        pos,
                        "`default` label is already defined in switch".to_string(),
                    );
                }
                let label = c.new_label();
                case_label_indices.push(label);
                c.label_indices[label] = body_inst.len();
                default_label = Some(label);
            }
            SwInner::Case(pos, value) => {
                if used_case_values.contains(value) {
                    return CompErr::err(
                        pos,
                        format!("case {} is already defined in switch", value),
                    );
                }
                used_case_values.insert(value);

                let (case_loc, _) = gen_int(c, &mut body_inst, *value, case_reg);
                let label = c.new_label();
                case_label_indices.push(label);
                c.label_indices[label] = body_inst.len();
                instructions.push(opcode_gen_cmp(c, &case_loc, &cond_loc));
                instructions.push(opcode_gen_jump(Jump::Je, label));
            }
            SwInner::Statement(body) => gen_statement(c, &mut body_inst, body)?,
        }
    }
    c.break_dest_stack.pop();

    let inst = match default_label {
        Some(label) => opcode_gen_jump(Jump::Jmp, label),
        None => opcode_gen_jump(Jump::Jmp, switch_end_label),
    };

    // Default jump point
    instructions.push(inst);
    for index in case_label_indices {
        c.label_indices[index] += instructions.len();
    }
    instructions.append(&mut body_inst);
    c.label_indices[switch_end_label] = instructions.len();
    Ok(())
}

fn gen_auto(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    pos: &Pos,
    vars: &Vec<Var>,
) -> Result<(), CompErr> {
    for var in vars {
        // Guaranteed to exist because of the prepass
        let dest_loc = c.local_var_locs.get(var.name()).unwrap().clone();
        c.add_to_scope(pos, var.name().clone(), ScopeEntry::Var(dest_loc.clone()))?;

        match var {
            Var::Vec(_, size, initial) => {
                let offset = if let Loc::Stack(offset) = dest_loc {
                    offset
                } else {
                    panic!("Auto loc should always be on the stack!");
                };

                // The first value in the stack for a vector is a data pointer
                instructions.push(opcode_gen_lea(&Loc::Stack(offset - size), Reg::Rax));
                instructions.push(opcode_gen_mov(c, &Reg::Rax.into(), &dest_loc));

                for (i, value) in initial.iter().enumerate() {
                    let val_dest_loc = Loc::Stack(offset - size + i as i64);

                    let (val_loc, _) = gen_int(c, instructions, *value, Reg::Rax);
                    instructions.push(opcode_gen_mov(c, &val_loc, &val_dest_loc));
                }
            }
            Var::Single(_, Some(value)) => {
                let (val_loc, _) = gen_int(c, instructions, *value, Reg::Rax);
                instructions.push(opcode_gen_mov(c, &val_loc, &dest_loc));
            }
            Var::Single(_, None) => {}
        }
    }
    Ok(())
}

fn gen_extern(c: &mut FunContext, pos: &Pos, vars: &Vec<String>) -> Result<(), CompErr> {
    for name in vars {
        if c.find_in_scope(name).is_some() {
            return CompErr::err(pos, format!("{} is already is scope", name));
        }

        match c.global_scope.get(name) {
            Some(ScopeEntry::Var(Loc::Data(_))) => {
                let entry = ScopeEntry::Var(Loc::Data(name.clone()));
                c.add_to_scope(pos, name.clone(), entry)?;
            }
            Some(ScopeEntry::Fun(_)) => {
                return CompErr::err(pos, format!("{} is a function, not a global var", name))
            }
            _ => return CompErr::err(pos, format!("Could not find definition for {}", name)),
        }
    }
    Ok(())
}

fn gen_statement(
    c: &mut FunContext,
    instructions: &mut Vec<Inst>,
    body: &Statement,
) -> Result<(), CompErr> {
    match body {
        Statement::Null => Ok(()),
        Statement::Break(pos) => match c.break_dest_stack.last() {
            Some(label) => {
                instructions.push(opcode_gen_jump(Jump::Jmp, *label));
                Ok(())
            }
            None => CompErr::err(pos, "Cannot break from this location".to_string()),
        },
        Statement::Goto(pos, name) => match c.labels.get(name) {
            Some(label) => {
                instructions.push(opcode_gen_jump(Jump::Jmp, *label));
                Ok(())
            }
            None => CompErr::err(
                pos,
                format!("Label '{}' not defined in this function", name),
            ),
        },
        // We preprocess the labels, so we know it must exist
        Statement::Label(_, name) => {
            let l = c.labels.get(name).unwrap();
            c.label_indices[*l] = instructions.len();
            Ok(())
        }
        Statement::Return => {
            gen_return(instructions);
            Ok(())
        }
        Statement::ReturnExpr(expr) => {
            gen_return_expr(c, instructions, expr)
        }
        Statement::Block(statements) => {
            c.new_scope();
            for statement in statements {
                gen_statement(c, instructions, statement)?;
            }
            c.drop_scope();
            Ok(())
        }
        Statement::Auto(pos, vars) => {
            gen_auto(c, instructions, pos, vars)
        }
        Statement::Extern(pos, vars) => {
            gen_extern(c, pos, vars)
        }
        Statement::Expr(expr) => {
            gen_expr(c, instructions, expr)?;
            Ok(())
        }
        Statement::If(cond, if_body, None) => {
            gen_if(c, instructions, cond, if_body)
        }
        Statement::If(cond, if_body, Some(else_body)) => {
            gen_if_else(c, instructions, cond, if_body, else_body)
        }
        Statement::While(cond, body) => {
            gen_while(c, instructions, cond, body)
        }
        Statement::Switch(cond, body) => {
            gen_switch(c, instructions, cond, body)
        }
    }
}

/// Returns the function instructions & number of bytes in the function
fn gen_fun(c: &mut FunContext, function: &RSFunction) -> Result<Vec<Inst>, CompErr> {
    let pos = &function.pos;
    let args = &function.args;
    let body = &function.body;

    c.new_scope();
    let mut instructions = vec![];
    // Save base pointer, since it's callee-saved
    instructions.push(opcode_gen_push(&Loc::Register(Reg::Rbp)));
    instructions.push(opcode_gen_mov(c, &Reg::Rsp.into(), &Reg::Rbp.into()));

    // Prepare initial stack memory
    alloc_args(c, &mut instructions, pos, args)?;
    prepass_gen(
        c,
        &mut instructions,
        body,
        1 + std::cmp::min(6, args.len() as i64),
    )?;

    gen_statement(c, &mut instructions, body)?;

    let trailing_ret = match instructions.last() {
        Some(instruction) => instruction[0] == 0xc3,
        _ => false,
    };

    if !trailing_ret {
        gen_return(&mut instructions);
    }
    c.drop_scope();

    // Put proper relative jump offsets onto jump instructions.
    // Initially, we populate with label indexes, since we don't know where a
    // label will be until the entire function is generated.
    // TODO: Shrink jump instructions to 2-byte alternative when possible
    let mut label_offsets = vec![];
    for label in &c.label_indices {
        label_offsets.push(inst_offset(&instructions, *label));
    }
    let mut inst_offset = 0;
    for inst in instructions.iter_mut() {
        inst_offset += inst.len() as i64;
        if is_jump(inst) {
            let label_index = inst_32_tail(inst);
            inst.truncate(inst.len() - 4);
            let label_offset = label_offsets[label_index];
            append_le32_bytes(inst, label_offset - inst_offset);
        }
    }
    Ok(instructions)
}

// Prepass to find the global scope, before we start evaluating things
fn root_prepass(
    functions: &Vec<RSFunction>,
    variables: &Vec<RSVariable>,
    defines: Vec<RSDefine>,
) -> Result<HashMap<String, ScopeEntry>, CompErr> {
    let mut scope = HashMap::new();
    for variable in variables {
        let var = &variable.var;

        let name = var.name();
        if scope.contains_key(name) {
            return CompErr::err(&variable.pos, format!("{} already in root scope", name));
        }
        scope.insert(name.clone(), ScopeEntry::Var(Loc::Data(name.clone())));
    }
    for function in functions {
        let name = &function.name;
        if scope.contains_key(name) {
            return CompErr::err(&function.pos, format!("{} already in root scope", name));
        }
        scope.insert(name.clone(), ScopeEntry::Fun(function.args.len()));
    }
    for define in defines {
        let name = &define.name;
        if scope.contains_key(name) {
            return CompErr::err(&define.pos, format!("{} already in root scope", name));
        }

        scope.insert(name.clone(), ScopeEntry::Define(define.args, define.body));
    }
    Ok(scope)
}

fn elf_gen_elf_header(c: &ElfContext) -> Vec<u8> {
    let mut bytes = vec![
        // Magic ELF header
        0x7f, 0x45, 0x4c, 0x46,
        // 64-bit
        0x02,
        // Little endian
        0x01,
        // Version (always 1)
        0x01,
        // System V format
        0x00,
        // ABI version
        0x00,
        // Padding
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        // e_type (executable file)
        0x02, 0x00,
        // e_machine (AMD x86-64)
        0x3e, 0x00,
        // e_version (1 for original ELF version)
        0x01, 0x00, 0x00, 0x00,
    ];
    // e_entry
    append_le64_bytes(&mut bytes, c.program_vaddr());
    // e_phoff: Program header offset in the generated file
    append_le64_bytes(&mut bytes, ELF_HEADER_SIZE);
    // e_shoff
    append_le64_bytes(&mut bytes, c.section_header_offset());
    // e_flags
    append_le32_bytes(&mut bytes, 0);
    // e_ehsize
    append_le16_bytes(&mut bytes, ELF_HEADER_SIZE);
    // e_phentsize
    append_le16_bytes(&mut bytes, PROGRAM_HEADER_SIZE);
    // e_phnum: We only ever have a single program header
    append_le16_bytes(&mut bytes, PROGRAM_HEADER_COUNT);
    // e_shentsize
    append_le16_bytes(&mut bytes, SECTION_HEADER_SIZE);
    // e_shnum
    append_le16_bytes(&mut bytes, SECTION_HEADER_COUNT);
    // e_shstrndx
    append_le16_bytes(&mut bytes, SHTAB_SECTION_INDEX);
    bytes
}

fn elf_gen_program_headers(c: &ElfContext) -> Vec<u8> {
    let mut bytes = vec![];

    ///////////////////
    // rodata header //
    ///////////////////

    // p_type: Loadable segment
    append_le32_bytes(&mut bytes, 0x01);
    // p_flags
    append_le32_bytes(&mut bytes, 0x04);
    // p_offset: Offset in the file image of the program opcodes
    append_le64_bytes(&mut bytes, c.rodata_offset());
    // p_vaddr: Virtual address of the segment in memory
    append_le64_bytes(&mut bytes, c.rodata_vaddr());
    // p_paddr: Physical address of the segment in memory
    append_le64_bytes(&mut bytes, c.rodata_vaddr());
    // p_filesz: Size in bytes of the segment in file image
    append_le64_bytes(&mut bytes, c.rodata_size);
    // p_memsz: Size in bytes of the segment in memory
    append_le64_bytes(&mut bytes, c.rodata_size);
    // p_align: Required section alignment
    append_le64_bytes(&mut bytes, PAGE_SIZE);

    /////////////////
    // data header //
    /////////////////

    // p_type: Loadable segment
    append_le32_bytes(&mut bytes, 0x01);
    // p_flags
    append_le32_bytes(&mut bytes, 0x06);
    // p_offset: Offset in the file image of the program opcodes
    append_le64_bytes(&mut bytes, c.data_offset());
    // p_vaddr: Virtual address of the segment in memory
    append_le64_bytes(&mut bytes, c.data_vaddr());
    // p_paddr: Physical address of the segment in memory
    append_le64_bytes(&mut bytes, c.data_vaddr());
    // p_filesz: Size in bytes of the segment in file image
    append_le64_bytes(&mut bytes, c.data_size);
    // p_memsz: Size in bytes of the segment in memory
    append_le64_bytes(&mut bytes, c.data_size);
    // p_align: Required section alignment
    append_le64_bytes(&mut bytes, PAGE_SIZE);

    /////////////////
    // text header //
    /////////////////

    // p_type: Loadable segment
    append_le32_bytes(&mut bytes, 0x01);
    // p_flags
    append_le32_bytes(&mut bytes, 0x05);
    // p_offset: Offset in the file image of the program opcodes
    append_le64_bytes(&mut bytes, c.program_offset());
    // p_vaddr: Virtual address of the segment in memory
    append_le64_bytes(&mut bytes, c.program_vaddr());
    // p_paddr: Physical address of the segment in memory
    append_le64_bytes(&mut bytes, c.program_vaddr());
    // p_filesz: Size in bytes of the segment in file image
    append_le64_bytes(&mut bytes, c.program_size);
    // p_memsz: Size in bytes of the segment in memory
    append_le64_bytes(&mut bytes, c.program_size);
    // p_align: Required section alignment
    append_le64_bytes(&mut bytes, PAGE_SIZE);

    bytes
}

fn elf_gen_section_headers(c: &ElfContext) -> Vec<u8> {
    assert!(c.program_size != 0);

    // Initially populate with the mandator null header
    let mut bytes = vec![0; SECTION_HEADER_SIZE as usize];

    ///////////////////////
    // .shstrtab section //
    ///////////////////////
    // sh_name: Offset to a string in .shstrtab section
    append_le32_bytes(&mut bytes, 0x01);
    // sh_type
    append_le32_bytes(&mut bytes, 0x03);
    // sh_flags
    append_le64_bytes(&mut bytes, 0x00);
    // sh_addr: Virtual address of the section in memory, for loaded sections
    append_le64_bytes(&mut bytes, 0);
    // sh_offset: Offset in the file image of this section
    append_le64_bytes(&mut bytes, c.shstrtab_offset());
    // sh_size: Size in bytes of the section in the file image
    append_le64_bytes(&mut bytes, c.shstrtab_size);
    // sh_link
    append_le32_bytes(&mut bytes, 0);
    // sh_info
    append_le32_bytes(&mut bytes, 0);
    // sh_addralign
    append_le64_bytes(&mut bytes, 0);
    // sh_entsize
    append_le64_bytes(&mut bytes, 0);

    //////////////////////////
    // Program data section //
    //////////////////////////

    // sh_name: Offset to a string in .shstrtab section
    append_le32_bytes(&mut bytes, 0x0b);
    // sh_type
    append_le32_bytes(&mut bytes, 0x01);
    // sh_flags
    append_le64_bytes(&mut bytes, 0x06);
    // sh_addr: Virtual address of the section in memory, for loaded sections
    append_le64_bytes(&mut bytes, c.program_vaddr());
    // sh_offset: Offset in the file image of this section
    append_le64_bytes(&mut bytes, c.program_offset());
    // sh_size: Size in bytes of the section in the file image
    append_le64_bytes(&mut bytes, c.program_size);
    // sh_link
    append_le32_bytes(&mut bytes, 0);
    // sh_info
    append_le32_bytes(&mut bytes, 0);
    // sh_addralign
    append_le64_bytes(&mut bytes, 1);
    // sh_entsize
    append_le64_bytes(&mut bytes, 0);

    ///////////////////
    // .data section //
    ///////////////////
    // sh_name: Offset to a string in .shstrtab section
    append_le32_bytes(&mut bytes, 0x19);
    // sh_type
    append_le32_bytes(&mut bytes, 0x01);
    // sh_flags
    append_le64_bytes(&mut bytes, 0x03);
    // sh_addr: Virtual address of the section in memory, for loaded sections
    append_le64_bytes(&mut bytes, c.data_vaddr());
    // sh_offset: Offset in the file image of this section
    append_le64_bytes(&mut bytes, c.data_offset());
    // sh_size: Size in bytes of the section in the file image
    append_le64_bytes(&mut bytes, c.data_size);
    // sh_link
    append_le32_bytes(&mut bytes, 0);
    // sh_info
    append_le32_bytes(&mut bytes, 0);
    // sh_addralign
    append_le64_bytes(&mut bytes, 1);
    // sh_entsize
    append_le64_bytes(&mut bytes, 0);

    /////////////////////
    // .rodata section //
    /////////////////////
    // sh_name: Offset to a string in .shstrtab section
    append_le32_bytes(&mut bytes, 0x11);
    // sh_type
    append_le32_bytes(&mut bytes, 0x01);
    // sh_flags
    append_le64_bytes(&mut bytes, 0x02);
    // sh_addr: Virtual address of the section in memory, for loaded sections
    append_le64_bytes(&mut bytes, c.rodata_vaddr());
    // sh_offset: Offset in the file image of this section
    append_le64_bytes(&mut bytes, c.rodata_offset());
    // sh_size: Size in bytes of the section in the file image
    append_le64_bytes(&mut bytes, c.rodata_size);
    // sh_link
    append_le32_bytes(&mut bytes, 0);
    // sh_info
    append_le32_bytes(&mut bytes, 0);
    // sh_addralign
    append_le64_bytes(&mut bytes, 1);
    // sh_entsize
    append_le64_bytes(&mut bytes, 0);

    bytes
}

/// Returns the string bytes, and a mapping from the string_id -> vaddr
fn elf_gen_rodata(
    c: &ElfContext,
    strings: Vec<(usize, Vec<u8>)>,
) -> (Vec<u8>, Arc<HashMap<usize, i64>>) {
    let base_vaddr = c.rodata_vaddr();
    let mut bytes = vec![];
    let mut vaddrs = HashMap::<usize, i64>::new();
    for (string_id, string_bytes) in strings {
        vaddrs.insert(string_id, base_vaddr + bytes.len() as i64);
        bytes.extend(string_bytes);
        bytes.push(0);
    }
    (bytes, Arc::new(vaddrs))
}

fn elf_gen_data(
    c: &ElfContext,
    variables: Vec<RSVariable>,
) -> (Vec<u8>, Arc<HashMap<String, i64>>) {
    let base_vaddr = c.data_vaddr();
    let mut bytes = vec![];
    let mut vaddrs = HashMap::<String, i64>::new();
    for var in variables {
        let vaddr = base_vaddr + bytes.len() as i64;
        match var.var {
            Var::Vec(name, capacity, values) => {
                let uninitialized_num = capacity - values.len() as i64;
                vaddrs.insert(name, vaddr);
                // First element is actually a pointer to the first element
                // This is so that global vectors behave the same as local vecs
                append_le64_bytes(&mut bytes, vaddr + 8);
                for value in values {
                    append_le64_bytes(&mut bytes, value);
                }
                for _ in 0..uninitialized_num {
                    append_le64_bytes(&mut bytes, 0);
                }
            },
            Var::Single(name, value) => {
                vaddrs.insert(name, vaddr);
                append_le64_bytes(&mut bytes, value.unwrap_or(0));
            },
        }
    }
    (bytes, Arc::new(vaddrs))
}

/// Crappy function but will be removed eventually so I don't care
fn offset_after_instruction(
    instructions: &[Inst],
    inst_index: i64,
) -> i64 {
    let mut offset = 0;
    for inst in instructions.iter().take(inst_index as usize + 1) {
        offset += inst.len();
    }
    offset as i64
}

fn gen(
    strings: Vec<(usize, Vec<u8>)>,
    functions: Vec<RSFunction>,
    variables: Vec<RSVariable>,
    defines: Vec<RSDefine>,
    writer: &mut dyn Write,
) -> Result<(), CompErr> {
    let mut w = BufWriter::new(writer);

    let global_scope = root_prepass(&functions, &variables, defines)?;

    let shstrtab_bytes = "\0.shstrtab\
                          \0.text\
                          \0.rodata\
                          \0.data\
                          \0.symtab\
                          \0.strtab\0".as_bytes();
    let shstrtab_size = shstrtab_bytes.len();
    let mut elf_context = ElfContext{
        rodata_size: 0,
        data_size: 0,
        program_size: 0,
        shstrtab_size: shstrtab_size as i64,
    };
    let (rodata_bytes, rodata_vaddrs) = elf_gen_rodata(&elf_context, strings);
    elf_context.rodata_size = rodata_bytes.len() as i64;
    let (data_bytes, data_vaddrs) = elf_gen_data(&elf_context, variables);
    elf_context.data_size = data_bytes.len() as i64;

    let pool = Arc::new((
        Mutex::new(CodeGenPool {
            running_fibers: 0,
            functions,
            results: vec![],
            errors: vec![],
        }),
        Condvar::new(),
    ));

    let thread_count = logical_cpu_count();
    let arc_global_scope = Arc::new(global_scope);

    let mut handles = Vec::with_capacity(thread_count);
    for _ in 0..thread_count {
        let th_pool_mutex = pool.clone();
        let th_global_scope = arc_global_scope.clone();
        let th_rodata_vaddrs = rodata_vaddrs.clone();
        let th_data_vaddrs = data_vaddrs.clone();

        handles.push(thread::spawn(move || {
            codegen_fiber(th_global_scope, th_rodata_vaddrs, th_data_vaddrs, th_pool_mutex);
        }))
    }

    for handle in handles {
        handle.join().unwrap();
    }
    let mut guard = pool.0.lock().unwrap();
    if !guard.errors.is_empty() {
        return Err(guard.errors[0].clone());
    }

    let mut fun_offsets = HashMap::<String, usize>::new();
    let mut inst_offset = 0;
    for (fun_name, instructions, _) in &mut guard.results {
        fun_offsets.insert(fun_name.clone(), inst_offset);
        for instruction in instructions {
            inst_offset += instruction.len();
        }
    }

    // +9 since that's the number of bytes past the main call in _start
    let main_offset = *fun_offsets.get("main").unwrap() + 9;
    // Generate the _start function
    // TODO: Code to initialize vec pointers in the data segment
    // Better yet: compute them during data segment initialization
    // We already know the vaddrs of the entire data segment, so we shouldn't
    // have to use instruction relative addressing at all for vector data!!
    let mut all_instructions: Vec<u8> = vec![
        // mov    (%rsp),%rdi
        0x48, 0x8b, 0x3c, 0x24,
        // lea    0x8(%rsp),%rsi
        0x48, 0x8d, 0x74, 0x24, 0x08,
        // call main
        0xe8,
        main_offset as u8,
        (main_offset >> 8) as u8, 
        (main_offset >> (8*2)) as u8, 
        (main_offset >> (8*3)) as u8, 
        // mov    %al,%bh
        0x88, 0xc7,
        // mov    $0x3c,%rax
        0x48, 0x31, 0xc0, 0xb0, 0x3c,
        // syscall
        0x0f, 0x05,
    ];

    let mut total_offset = 0;
    for (_, instructions, inst_links) in &mut guard.results {
        for (inst_index, symbol) in inst_links {
            let inst_offset = total_offset + offset_after_instruction(instructions, *inst_index as i64);
            // FIXME: Something jank is going on here... with instruction offsets
            // Idk if I care enough to fix it, since I'm going to change this soon anyways
            // println!("a. {:?}", instructions[*inst_index-1]);
            // println!("b. {:?}", instructions[*inst_index-0]);
            // println!("c. {:?}", instructions[*inst_index+1]);
            let instruction = &mut instructions[*inst_index];
            if instruction.is_empty() {
                panic!("No opcodes for instruction: {:?}", instruction);
            }
            instruction.truncate(instruction.len() - 4);
            let callee_offset = *fun_offsets.get(symbol).unwrap() as i64;
            append_le32_bytes(instruction, callee_offset - inst_offset);
        }
        for instruction in instructions {
            if instruction.is_empty() {
                panic!("No opcodes for instruction: {:?}", instruction);
            }
            all_instructions.extend(instruction.as_slice());
            total_offset += instruction.len() as i64;
        }
    }
    elf_context.program_size = all_instructions.len() as i64;

    let elf_header = elf_gen_elf_header(&elf_context);
    let program_headers = elf_gen_program_headers(&elf_context);
    let section_headers = elf_gen_section_headers(&elf_context);
    let header_size = elf_header.len() + program_headers.len() + shstrtab_size;

    assert!(header_size <= PAGE_SIZE as usize);

    CompErr::from_io_res(w.write_all(&elf_header))?;
    CompErr::from_io_res(w.write_all(&program_headers))?;
    CompErr::from_io_res(w.write_all(shstrtab_bytes))?;

    // rodata
    let mut padding = PAGE_SIZE - header_size as i64;
    CompErr::from_io_res(w.write_all(&vec![0; padding as usize]))?;
    CompErr::from_io_res(w.write_all(&rodata_bytes))?;

    // data
    padding = (PAGE_SIZE - elf_context.rodata_size) % PAGE_SIZE;
    CompErr::from_io_res(w.write_all(&vec![0; padding as usize]))?;
    CompErr::from_io_res(w.write_all(&data_bytes))?;

    // text (opcodes)
    padding = (PAGE_SIZE - elf_context.data_size) % PAGE_SIZE;
    CompErr::from_io_res(w.write_all(&vec![0; padding as usize]))?;
    CompErr::from_io_res(w.write_all(&all_instructions))?;

    // Section headers must be at the end of the file, sadly
    CompErr::from_io_res(w.write_all(&section_headers))?;
    Ok(())
}

fn unpool_function(pool: &Arc<(Mutex<CodeGenPool>, Condvar)>) -> Option<RSFunction> {
    let mut guard = pool.0.lock().unwrap();

    if !guard.errors.is_empty() {
        return None;
    }

    guard.functions.pop().map(|fun| {
        guard.running_fibers += 1;
        fun
    })
}

fn codegen_fiber(
    global_scope: Arc<HashMap<String, ScopeEntry>>,
    str_vaddrs: Arc<HashMap<usize, i64>>,
    data_vaddrs: Arc<HashMap<String, i64>>,
    pool: Arc<(Mutex<CodeGenPool>, Condvar)>,
) {
    loop {
        match unpool_function(&pool) {
            Some(fun) => {
                let mut c = FunContext {
                    global_scope: &global_scope,
                    str_vaddrs: &str_vaddrs,
                    data_vaddrs: &data_vaddrs,
                    fun_scope: HashMap::new(),
                    block_vars: vec![],
                    local_var_locs: HashMap::new(),
                    labels: HashMap::new(),
                    label_counter: 0,
                    break_dest_stack: vec![],
                    label_indices: vec![],
                    inst_links: vec![],
                };
                match gen_fun(&mut c, &fun) {
                    Ok(instructions) => {
                        let (mutex, cvar) = pool.as_ref();
                        let mut guard = mutex.lock().unwrap();
                        guard.results.push((fun.name, instructions, c.inst_links));
                        guard.running_fibers -= 1;
                        cvar.notify_all();
                    }
                    Err(err) => {
                        let (mutex, cvar) = pool.as_ref();
                        let mut guard = mutex.lock().unwrap();
                        guard.errors.push(err);
                        guard.running_fibers -= 1;
                        cvar.notify_all();
                    }
                }
            }
            None => {
                pool.1.notify_all();
                return;
            }
        }
    }
}

pub fn generate(parse_result: ParseResult, writer: &mut dyn Write) {
    match gen(
        parse_result.strings,
        parse_result.functions,
        parse_result.variables,
        parse_result.defines,
        writer,
    ) {
        Ok(_) => {}
        Err(err) => {
            print_comp_error(&parse_result.file_paths, &err);
            std::process::exit(1);
        }
    }
}
