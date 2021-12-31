#import "../assets/best.b";

a() {
    return(2 << (3 * 4));
}

b() {
    return(2 << (3 << 2));
}

main() {
    assert_eq_int(8192, a());
    assert_eq_int(8192, b());
}