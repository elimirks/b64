#import "../assets/best.b";

#define THE_ANSWER 42;
#define LUCKY (13);
#define SEMIPRIME() 35;
#define BINCEIL(n, m) (n + m) & ~m;

main() {
    assert_eq_int(42, THE_ANSWER);
    assert_eq_int(13, LUCKY());
    assert_eq_int(35, SEMIPRIME);
    assert_eq_int(8, BINCEIL(3, 7));
}