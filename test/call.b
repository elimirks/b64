#import "../assets/best.b";

main() {
    foo(0, 1, 2, 3, 4, 5, 6, 7, 8, 9);
}

foo(a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) {
    assert_eq_int(0, a0);
    assert_eq_int(1, a1);
    assert_eq_int(2, a2);
    assert_eq_int(3, a3);
    assert_eq_int(4, a4);
    assert_eq_int(5, a5);
    assert_eq_int(6, a6);
    assert_eq_int(7, a7);
    assert_eq_int(8, a8);
    assert_eq_int(9, a9);
}