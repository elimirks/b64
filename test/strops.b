#import "../assets/best.b";

test_strcmp() {
    assert_eq_int(-1, strcmp("aaaaaaaaac", "aaaaaaaaab"));
    assert_eq_int(1, strcmp("", "eouotheusntaoeusntaehutsnahtoneuah"));
    assert_eq_int(1, strcmp("a", "e"));
    assert_eq_int(1, strcmp("a", "eouotheusntaoeusntaehutsnahtoneuah"));
    assert_eq_int(-1, strcmp("z", "e"));
    assert_eq_int(1, strcmp("z", "eouotheusntaoeusntaehutsnahtoneuah"));
}

test_strcat() {
    assert_eq_str(
        "Hello Mars, goodbye Earth*n",
        strcat("Hello Mars, ", "goodbye Earth*n")
    );
}

main() {
    test_strcmp();
    test_strcat();
}
