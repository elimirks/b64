#import "../assets/best.b";

doubleplus1(xref) {
    *xref = *xref + *xref;
    *xref = 1 + **&xref;
}

testDeref() {
    auto x;
    x = 3;
    doubleplus1(&x);
    assert_eq_int(7, x);
}

main() {
    testDeref();

/*
    auto x, y 0;
    x = &y;
    *x =+ 1 + 1;
    assert_eq_int(2, y);
    *x =+ 1 * 1;
    assert_eq_int(3, y);
*/
}
