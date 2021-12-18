/* Returns nonsense for n < 0 */
fib(n) {
  return(
    n <= 1
        ? n
        : fib(n - 1) + fib(n - 2)
  );
}

main() {
    auto x;
    x = 2;
    /* Should return "55" */
    return(fib(10));
}

/* Some utf-8 Ω */