main() {
    /* No vectors yet! Gotta make due with char pointers */
    auto first, last;
    first = 'Hello wo';
    last  = 'rld!*n';
    syscall(1, 1, &first, 8);
    syscall(1, 1, &last, 5);
}