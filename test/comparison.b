#import "../assets/best.b";

main() {
    auto x;
    x = 42;
    assert_eq_int(1, 24 == 12 * 2);
    assert_eq_int(1, 24 >= 24);
    assert_eq_int(1, 24 >= 23);
    assert_eq_int(1, 24 <= 24);
    assert_eq_int(1, 24 <= 25);

    assert_eq_int(1, x == 42);
    assert_eq_int(1, 42 == x);
}

