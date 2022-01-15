#import "../assets/best.b";

test_multi_auto_vec() {
    printf("Testing a multiple vecs on the stack*n");
    auto as[4];
    auto bs[4];

    /* To check they don't get overridden */
    auto a_ptr, b_ptr;
    a_ptr = as;
    b_ptr = bs;

    as[0] = 0;
    as[1] = 1;
    as[2] = 2;
    as[3] = 3;
    as[4] = 4;

    bs[0] = 0;
    bs[1] = 1;
    bs[2] = 2;
    bs[3] = 3;
    bs[4] = 4;

    /* Making sure vec pointers didn't get overwritten */
    assert_eq_int(a_ptr, as);
    assert_eq_int(b_ptr, bs);

    assert_eq_int(0, as[0]);
    assert_eq_int(1, as[1]);
    assert_eq_int(2, as[2]);
    assert_eq_int(3, as[3]);
    assert_eq_int(4, as[4]);

    assert_eq_int(0, bs[0]);
    assert_eq_int(1, bs[1]);
    assert_eq_int(2, bs[2]);
    assert_eq_int(3, bs[3]);
    assert_eq_int(4, bs[4]);
}

test_mono_auto_vec() {
    printf("Testing a single vec on the stack*n");
    auto as[4];

    /* To check they don't get overridden */
    auto a_ptr;
    a_ptr = as;

    as[0] = 0;
    as[1] = 1;
    as[2] = 2;
    as[3] = 3;
    as[4] = 4;

    /* Making sure vec pointers didn't get overwritten */
    assert_eq_int(a_ptr, as);

    assert_eq_int(0, *as);
    assert_eq_int(1, as[1]);
    assert_eq_int(2, as[2]);
    assert_eq_int(3, as[3]);
    assert_eq_int(4, as[4]);
}

main() {
    auto nums[5] 2;
    *(nums + 8) = 3;
    nums[2] = 5;

    assert_eq_int(2, *nums);
    assert_eq_int(3, *(nums + 8));
    assert_eq_int(5, nums[2]);

    test_mono_auto_vec();
    test_multi_auto_vec();
    return(0);
}