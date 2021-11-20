charlen(c) {
    auto len;
    len = 0;
    while (c) {
        c = c >> 8;
        len = len + 1;
    }
    return(len);
}

putchar(c) {
    syscall(1, 1, &c, charlen(c));
}

main() {
    putchar('Hello wo');
    putchar('rld!*n');
}