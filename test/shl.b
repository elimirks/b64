a() {
    return(2 << (3 * 4));
}

b() {
    return(2 << (3 << 2));
}

main() {
    /* To assert they're the same */
    return(a() - b());
}