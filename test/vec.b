main() {
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