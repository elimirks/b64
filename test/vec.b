main() {
    auto nums[3];
    nums[0] = 2;
    *(nums + 8) = 3;
    nums[2] = 5;

    putnum(*nums);
    putchar('*n');
    putnum(*(nums + 8));
    putchar('*n');
    putnum(nums[2]);
    putchar('*n');
}