#import "../assets/best.b";

main() {
    assert_eq_int(42, atoi("42"));
    assert_eq_int(0, atoi("x42"));
    assert_eq_int(1234567890, atoi("1234567890"));
    assert_eq_int(42, atoi("42abc"));
    assert_eq_int(-42, atoi("-42"));
    assert_eq_int(0, atoi("-"));
    assert_eq_int(0, atoi("-abch1234"));
}
