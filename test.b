/*
 * https://cs61.seas.harvard.edu/site/2018/Asm2/
 * https://cs.brown.edu/courses/cs033/docs/guides/x64_cheatsheet.pdf
 */
/* Assumes n >= 0 */
fib(n) {
  if (n <= 1) return(n);
  return(fib(n - 1) + fib(n - 2));
}

fastfib(n) {
  auto i, curr, prev;
  i = 1;
  prev = 0;
  curr = 1;

  /* TODO: Give the illusion of scope w.r.t. auto */
  while (i < n) {
    auto next;
    next = curr + prev;
    prev = curr;
    curr = next;
    i = i + 1;
  }

  return(curr);
}

main() {
    return(fastfib(12));
}