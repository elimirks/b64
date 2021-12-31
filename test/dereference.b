#import "../assets/best.b";

doubleplus1(xref) {
    *xref = *xref + *xref;
    *xref = 1 + **&xref;
}

main() {
    auto x;
    x = 3;
    doubleplus1(&x);
    assert_eq_int(7, x);
}