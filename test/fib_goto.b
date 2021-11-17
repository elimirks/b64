fib(n) {
  auto i, curr, prev, next;
  i = 1;
  prev = 0;
  curr = 1;

begin:
  if (i < n) return(curr);
  next = curr + prev;
  prev = curr;
  curr = next;
  i = i + 1;
  goto begin;

  return(curr);
}

main() {
    return(fib(12));
}
