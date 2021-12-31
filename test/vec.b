#import "../assets/best.b";

main() {
    auto nums[2] 2;
    *(nums + 8) = 3;
    nums[2] = 5;

    assert_eq_int(2, *nums);
    assert_eq_int(3, *(nums + 8));
    assert_eq_int(5, nums[2]);
}