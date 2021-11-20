main() {
    /* No vectors yet! Gotta make due with char pointers */
    auto h, e, l, o, newline;
    h = 72;
    e = 101;
    l = 108;
    o = 111;
    newline = 10;
    syscall(1, 1, &h, 1);
    syscall(1, 1, &e, 1);
    syscall(1, 1, &l, 1);
    syscall(1, 1, &l, 1);
    syscall(1, 1, &o, 1);
    syscall(1, 1, &newline, 1);
}