doubleplus1(xref) {
    *xref = *xref + *xref;
    *xref = 1 + **&xref;
}

main() {
    auto x;
    x = 3;
    doubleplus1(&x);
    /* Should return "7" */
    return(x);
}