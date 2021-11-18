.text
.global _start
_start:
    call main
    movq %rax,%rbx
    movq $1,%rax
    int $0x80

syscall:
    movq %rdi,%rax # Syscall number
    movq %rsi,%rbx
    xchg %rdx,%rcx
    movq %r8,%rsi
    movq %r9,%rdi
    int $0x80      # Returns via %rax already
    ret
