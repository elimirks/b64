#import "../assets/best.b";

main() {
    auto x, y;
    assert_eq_int(3, 2 / 2 + 2);
    assert_eq_int(3, 2 + 2 / 2);
    assert_eq_int(2, 8 / (x = y = 2 + 2));
    assert_eq_int(4, x);
    assert_eq_int(5, ++y);
    assert_eq_int(5, y);

    x = 5;
    y = 7;

    assert_eq_int(7, x =+ y = 2);
    assert_eq_int(7, x);
    assert_eq_int(2, y);

    x = 5;
    y = 7;
    assert_eq_int(9, x = y =+ 2);
    assert_eq_int(9, x);
    assert_eq_int(9, y);
}