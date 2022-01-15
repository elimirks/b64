#import "../assets/best.b";

test_charlen() {
    printf("Testing charlen*n");
    assert_eq_int(6, charlen('123456'));
    assert_eq_int(6, charlen('123456*08'));
    assert_eq_int(8, charlen('12345678'));
    assert_eq_int(0, charlen(''));
    assert_eq_int(charlen('123456'), charlen('123456*08'));
}

test_charcmp() {
    printf("Testing charcmp*n");
    assert_eq_int(0, charcmp('123456', '123456'));
    assert_eq_int(-1, charcmp('1234567', '123456'));
    assert_eq_int(1, charcmp('123456', '1234567'));
    assert_eq_int(1, charcmp('123456', '123457'));
    assert_eq_int(-1, charcmp('123457', '123456'));
    assert_eq_int(0, charcmp('123456', '123456*08'));
    assert_eq_int(0, charcmp('123456*08', '123456*07'));
    assert_eq_int(-1, charcmp('z', 'abc'));
}

test_strcmp() {
    printf("Testing strcmp*n");
    assert_eq_int(-1, strcmp("aaaaaaaaac", "aaaaaaaaab"));
    assert_eq_int(1, strcmp("", "eouotheusntaoeusntaehutsnahtoneuah"));
    assert_eq_int(1, strcmp("a", "e"));
    assert_eq_int(1, strcmp("a", "eouotheusntaoeusntaehutsnahtoneuah"));
    assert_eq_int(-1, strcmp("z", "e"));
    assert_eq_int(-1, strcmp("z", "anosethsntaeoee"));

    auto v[0] '123456*08';
    assert_eq_int(0, strcmp("123456", v));
}

test_strcat() {
    printf("Testing strcat*n");
    assert_eq_str(
        "Hello Mars, goodbye Earth*n",
        strcat("Hello Mars, ", "goodbye Earth*n")
    );

    auto v1[1] '12345678', '123456*07';

    assert_eq_str(
        "thing 12345678123456",
        strcat("thing ", v1)
    );

    auto v2[1] 'strops.b', '*0*0*0*01234';
    assert_eq_str(
        "/strops.b",
        strcat("/", v2)
    );
}

main() {
    test_charlen();
    test_charcmp();
    test_strcmp();
    test_strcat();
}
