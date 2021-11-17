fib(n) {
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
    return(fib(12));
}
