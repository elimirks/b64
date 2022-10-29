#import "../assets/best.b";

main() {
    assert_eq_int(9223372036854775807, 9223372036854775806 + 1);
    assert_eq_int(9223372036854775806, 9223372036854775807 - 1);
    assert_eq_int(6000000000000000000, 30000000000000000 * 200);
    assert_eq_int(-6000000000000000000, 30000000000000000 * (-200));
    assert_eq_int(-1234, 1 - 1235);
    assert_eq_int(8, 64 / 8);

    auto x 7;
    assert_eq_int(8, ++x);
    assert_eq_int(8, x++);
    assert_eq_int(9, x);
    assert_eq_int(8, --x);
    assert_eq_int(8, x--);
    assert_eq_int(7, x);
}
