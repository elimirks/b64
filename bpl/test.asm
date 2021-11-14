test:
  subq $8, %rsp   ; Allocate stack memory for `c`
  movq $3, (%rsp) ; c = 3
  subq $8, %rsp   ; Deallocate stack memory for c
  retq            ; Pop return address & jump there
