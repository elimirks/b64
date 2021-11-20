main() {
    /* No vectors yet! Gotta make due with char pointers */
    auto first, last, newline;
    first = 'Hello wo';
    last  = 'rld!';
    newline = 10;
    syscall(1, 1, &first, 8);
    syscall(1, 1, &last, 4);
    syscall(1, 1, &newline, 1);
}