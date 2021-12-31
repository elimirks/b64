/* B Test function library */
#import "stdlib.b";

/* All assert functions are of the form (expected, actual) */

assert_eq_int(a, b) {
    if (a != b) {
        printf("%d expected, %d given*n", a, b);
        exit(1);
    }
}

assert_eq_char(a, b) {
    if (a != b) {
        printf("'%c' expected, '%c' given*n", a, b);
        exit(1);
    }
}

assert_eq_str(a, b) {
    if (strcmp(a, b) != 0) {
        printf("*"%s*" expected, *"%s*" given*n", a, b);
        exit(1);
    }
}