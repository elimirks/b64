/* Some utf-8 comment Ω */
#import "../assets/best.b";

fib_goto(n) {
    auto i, curr, prev, next;
    i = 1;
    prev = 0;
    curr = 1;

begin:
    if (i >= n) return(curr);
    next = curr + prev;
    prev = curr;
    curr = next;
    i = i + 1;
    goto begin;
    return(curr);
}

fib_loop(n) {
    auto i, curr, prev;
    i = 1;
    prev = 0;
    curr = 1;

    while (i < n) {
        auto next;
        next = curr + prev;
        prev = curr;
        curr = next;
        i = i + 1;
    }
    return(curr);
}

/* Returns nonsense for n < 0 */
fib_rec_condexpr(n) {
    return(
        n <= 1
            ? n
            : fib_rec_condexpr(n - 1) + fib_rec_condexpr(n - 2)
    );
}

/* Returns nonsense for n < 0 */
fib_rec_switch(n) {
    switch (n) {
    case 0:  return(0);
    case 1:  return(1);
    default: return(fib_rec_switch(n - 1) + fib_rec_switch(n - 2));
    }
}

assert_fib_eq(expected, fib_index) {
    printf("Testing fib(%d) = %d*n", fib_index, expected);
    assert_eq_int(expected, fib_rec_switch(fib_index));
    assert_eq_int(expected, fib_rec_condexpr(fib_index));
    assert_eq_int(expected, fib_loop(fib_index));
    assert_eq_int(expected, fib_goto(fib_index));
}

main() {
    assert_fib_eq(1, 1);
    assert_fib_eq(1, 2);
    assert_fib_eq(2, 3);
    assert_fib_eq(3, 4);
    assert_fib_eq(5, 5);
    assert_fib_eq(8, 6);
    assert_fib_eq(13, 7);
    assert_fib_eq(21, 8);
    assert_fib_eq(34, 9);
}