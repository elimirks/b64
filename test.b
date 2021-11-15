/*
 * https://cs61.seas.harvard.edu/site/2018/Asm2/
 * https://cs.brown.edu/courses/cs033/docs/guides/x64_cheatsheet.pdf
 */
test(a) {
    auto c;
    c = a + 1;
    return(a + c);
}

add(a, b) {
  return(a + b);
}

/* Assumes n >= 0 */
fib(n) {
  if      (n == 0) return(0);
  else if (n == 1) return(1);

  return(fib(n - 1) + fib(n - 2));
}

main() {
    return(fib(10));
}