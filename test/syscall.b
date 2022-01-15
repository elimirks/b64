#import "../assets/best.b";

test_system() {
    printf("Checking return code of /bin/true*n");
    assert_eq_int(0, system("/bin/true", 0));
    printf("Checking return code of /bin/false*n");
    assert_eq_int(1, system("/bin/false", 0));
}

main() {
    test_system();
}