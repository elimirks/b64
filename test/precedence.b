main() {
    auto x, y;
    putnum(2 / 2 + 2); /* Should be 3 */
    putchar('*n');
    putnum(2 + 2 / 2); /* Should also be 3 */
    putchar('*n');
    putnum(8 / (x = y = 2 + 2));
    putchar('*n');
    putnum(x);
    putchar('*n');
    putnum(y);
    putchar('*n');
}