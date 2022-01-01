#import "../assets/best.b";

test_system() {
    assert_eq_int(0, system("/bin/true", 0));
    assert_eq_int(1, system("/bin/false", 0));
}

main() {
    test_system();
}