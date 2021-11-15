/*
 * https://cs61.seas.harvard.edu/site/2018/Asm2/
 * https://cs.brown.edu/courses/cs033/docs/guides/x64_cheatsheet.pdf
 */
test(a) {
    auto c;
    c = a + 1;
    return(a + c);
}

add(a, b) return(a + b);

main() {
    return(add(1, 3));
}