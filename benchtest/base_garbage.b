@import "../assets/stdlib.b";
@import "../assets/memory.b";

func1(xref) {
    *xref = *xref + *xref;
    *xref = 1 + **&xref;
}

func2() {
    auto x;
    x = 3;
    func1(&x);
    /* Should return "7" */
    return(x);
}

func3(n) {
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

func4() {
    return(func3(12));
}

func5(n) {
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

func6() {
    return(func5(12)); /* Should return 144 */
}

func7(n) {
  if (n <= 1) return(n);
  return(func7(n - 1) + func7(n - 2));
}

func8() {
    return(func7(10));
}

func9() {
    auto data1, data2, data3;

    data1 = malloc(4);
    data1[0] = 'aaaaaaaa';
    data1[1] = 'bbbbbbbb';
    data1[2] = 'cccccccc';
    data1[3] = 'dddddd*n';

    data2 = malloc(2);
    data2[0] = 'data2*n';

    putstr(data1);
    putstr(data2);

    printHeapMeta();
    free(data1);
    printHeapMeta();

    data3 = malloc(1);
    printHeapMeta();

    data3 = realloc(data3, 3);
    printHeapMeta();
}

func10() {
    auto c 10, i 3;
    putnum(i << 3);
    putstr("*n");
    putnum(i * 8);
    putstr("*n");
    putstr("Hello world!*n");

    putstr(num2str(-321));
    putstr("*n");
}

func11(n) {
    putnum(n);
    putchar('*n');
}

func12() {
    auto x, y;
    putstr("Should be 3: ");
    func11(2 / 2 + 2);

    putstr("Should be 3: ");
    func11(2 + 2 / 2);

    putstr("Should be 2: ");
    func11(8 / (x = y = 2 + 2));

    putstr("Should be 4: ");
    func11(x);

    putstr("Should be 5: ");
    func11(++y);

    putstr("Should be 5: ");
    func11(y);

    x = 5;
    y = 7;
    putstr("Should be 7 7 2:*n");
    func11(x =+ y = 2);
    func11(x);
    func11(y);

    x = 5;
    y = 7;
    putstr("Should be 9 9 9:*n");
    func11(x = y =+ 2);
    func11(x);
    func11(y);
}

func13() {
    putstr("strcmp:*n");
    putnum(strcmp("aaaaaaaaac", "aaaaaaaaab"));
    putstr("*n");
    putnum(strcmp("", "eouotheusntaoeusntaehutsnahtoneuah"));
    putstr("*n");
    putnum(strcmp("a", "e"));
    putstr("*n");
    putnum(strcmp("a", "eouotheusntaoeusntaehutsnahtoneuah"));
    putstr("*n");
    putnum(strcmp("z", "e"));
    putstr("*n");
    putnum(strcmp("z", "eouotheusntaoeusntaehutsnahtoneuah"));
    putstr("*n");
}

func14() {
    func13();
}

func15() {
    auto nums[2] 2;
    *(nums + 8) = 3;
    nums[2] = 5;

    putnum(*nums); /* Should print 2 */
    putchar('*n');
    putnum(*(nums + 8));
    putchar('*n');
    putnum(nums[2]);
    putchar('*n');
}