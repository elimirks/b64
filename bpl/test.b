test(a, b) {
  auto c;
  c = 3;
}


https://cs61.seas.harvard.edu/site/2018/Asm2/
https://cs.brown.edu/courses/cs033/docs/guides/x64_cheatsheet.pdf

test should compile into:

test:
  subq $8, %rsp   ; Allocate stack memory for `c`
  movq $3, (%rsp) ; c = 3
  subq $8, %rsp   ; Deallocate stack memory for c
  retq            ; Pop return address & jump there





add2(a, b) {
  auto c, d;
  d = 3;
  return(a + b + c);
}