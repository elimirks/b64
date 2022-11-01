#!/usr/bin/env python3

# Bytes based on the output of a stripped program that runs exit(42)

import os
import stat


def flatten(li):
    return [x for s in li for x in s]


def to_le64(number):
    return [
        (number >> 0x00) & 0xff,
        (number >> 0x08) & 0xff,
        (number >> 0x10) & 0xff,
        (number >> 0x18) & 0xff,
        (number >> 0x20) & 0xff,
        (number >> 0x28) & 0xff,
        (number >> 0x30) & 0xff,
        (number >> 0x38) & 0xff,
    ]


def to_le16(number):
    return [
        (number >> 0x00) & 0xff,
        (number >> 0x08) & 0xff,
    ]


elf_header_size = 0x40
program_header_size = 0x38
section_header_size = 0x40
required_alignment = 0x1000

virt_program_address = to_le64(0x400000)

# It seems this must be padded to align the prog data to 0x1000
prog_padding = bytes(8 * 497)

opcode_data = bytes([
    0x48, 0xc7, 0xc7, 0x2a, 0x00, 0x00, 0x00, 0x48,
    0xc7, 0xc0, 0x3c, 0x00, 0x00, 0x00, 0x0f, 0x05,
])
strtab_data = b'\0.shstrtab\0.text\0'
print(len(strtab_data))

prog_data = prog_padding + opcode_data + strtab_data

program_headers = [
    [
        # p_type: Loadable segment
        0x01, 0x00, 0x00, 0x00,
        # p_flags
        0x05, 0x00, 0x00, 0x00,
        # p_offset: Offset in the file image
        # Populated properly later
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        # p_vaddr: Virtual address of the segment in memory
    ] + virt_program_address + [
        # p_paddr: Physical address of the segment in memory
    ] + virt_program_address + [
        # p_filesz: Size in bytes of the segment in file image
        0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        # p_memsz: Size in bytes of the segment in memory
        0x10, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        # p_align: Required section alignment
    ] + to_le64(required_alignment),
]
prog_offset = len(flatten(program_headers)) + elf_header_size + len(prog_padding)
# Populate program data offset
program_headers[0][8:16] = to_le64(prog_offset)
program_header_bytes = bytes(flatten(program_headers))

section_headers = [
    # Mandatory null section
    [0] * section_header_size,
    # Program data section
    [
        # sh_name: Offset to a string in .shstrtab section
        0x0b, 0x00, 0x00, 0x00,
        # sh_type
        0x01, 0x00, 0x00, 0x00,
        # sh_flags
        0x06, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        # sh_addr: Virtual address of the section in memory, for loaded sections
    ] + virt_program_address + [
        # sh_offset: Offset in the file image of this section
    ] + to_le64(prog_offset) + [
        # sh_size: Size in bytes of the section in the file image
    ] + to_le64(len(opcode_data)) + [
        # sh_link
        0x00, 0x00, 0x00, 0x00,
        # sh_info
        0x00, 0x00, 0x00, 0x00,
        # sh_addralign
    ] + to_le64(required_alignment) + [
        # sh_entsize
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ],
    # .shstrtab section
    [
        # sh_name: Offset to a string in .shstrtab section
        0x01, 0x00, 0x00, 0x00,
        # sh_type
        0x03, 0x00, 0x00, 0x00,
        # sh_flags
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        # sh_addr: Virtual address of the section in memory, for loaded sections
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
        # sh_offset: Offset in the file image of this section
    ] + to_le64(prog_offset + len(opcode_data)) + [
        # sh_size: Size in bytes of the section in the file image
    ] + to_le64(len(strtab_data)) + [
        # sh_link
        0x00, 0x00, 0x00, 0x00,
        # sh_info
        0x00, 0x00, 0x00, 0x00,
        # sh_addralign
    ] + to_le64(required_alignment) + [
        # sh_entsize
        0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    ],
]
section_header_bytes = bytes(flatten(section_headers))

elf_header_bytes = bytes([
    # Magic ELF header
    0x7f, 0x45, 0x4c, 0x46,
    # 64-bit
    0x02,
    # Little endian
    0x01,
    # Version (always 1)
    0x01,
    # System V format
    0x00,
    # ABI version
    0x00,
    # Padding
    0x00, 0x00, 0x00, 0x00, 0x00, 0x00, 0x00,
    # e_type (executable file)
    0x02, 0x00,
    # e_machine (AMD x86-64)
    0x3e, 0x00,
    # e_version (1 for original ELF version)
    0x01, 0x00, 0x00, 0x00,
    # e_entry
] + virt_program_address + [
    # e_phoff: Program header offset in this file
] + to_le64(elf_header_size) + [
    # e_shoff: Section header offset in this file
] + to_le64(elf_header_size + len(program_header_bytes) + len(prog_data)) + [
    # e_flags
    0x00, 0x00, 0x00, 0x00,
    # e_ehsize
] + to_le16(elf_header_size) + [
    # e_phentsize
] + to_le16(program_header_size) + [
    # e_phnum
] + to_le16(len(program_headers)) + [
    # e_shentsize
] + to_le16(section_header_size) + [
    # e_shnum
] + to_le16(len(section_headers)) + [
    # e_shstrndx
    0x02, 0x00,
])

bytes = elf_header_bytes + program_header_bytes + prog_data + section_header_bytes

out_path = 'a.out'
with open(out_path, 'wb') as f:
    f.write(bytes)
os.chmod(out_path, stat.S_IRUSR | stat.S_IXUSR | stat.S_IWUSR)