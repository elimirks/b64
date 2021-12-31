#import "../assets/stdlib.b";

main() {
    auto nums[2] 2;
    *(nums + 8) = 3;
    nums[2] = 5;

    printf("%d*n", *nums); /* Should print 2 */
    printf("%d*n", *(nums + 8));
    printf("%d*n", nums[2]);
}