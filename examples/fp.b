#import "../assets/stdlib.b";

/* Some examples of functional programming in B */

/* Map the values in the given vector via the given function */
map(vector, count, fun_ptr) {
    auto i 0;
    while (i < count) {
        /* If an expression is NOT a name of a function, it is
         * considered a function pointer */
        vector[i] = fun_ptr(vector[i]);
        ++i;
    }
}

foreach(vector, count, fun_ptr) {
    auto i 0;
    while (i < count) {
        fun_ptr(vector[i]);
        ++i;
    }
}

fold(vector, count, initial_acc, fun_ptr) {
    auto acc, i 0;
    acc = initial_acc;
    while (i < count) {
        acc = fun_ptr(acc, vector[i]);
        ++i;
    }
    return(acc);
}

print_num(n) {
    printf("%d*n", n);
}

factorial(n) {
    switch (n) {
    case 0:  return(1);
    case 1:  return(1);
    default: return(n * factorial(n - 1));
    }
}

add(a, b) {
    return(a + b);
}

main() {
    auto numbers[] 0, 1, 2, 3, 4, 5, 6, 7, 8, 9;
    map(numbers, 10, &factorial);
    foreach(numbers, 10, &print_num);

    auto total;
    total = fold(numbers, 10, 0, &add);
    printf("Summation: %d*n", total);
}