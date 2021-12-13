/* Returns nonsense for n < 0 */
fib(n) {
  return(
    n <= 1
        ? n
        : fib(n - 1) + fib(n - 2)
  );
}

main() {
    return(fib(10));
}
