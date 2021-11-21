/**
 * Returns the length of the given wide char.
 */
charlen(c) {
    auto len;
    len = 0;
    while (c) {
        c = c >> 8;
        len = len + 1;
    }
    return(len);
}

/**
 * Writes each ASCII character in the given wide char to stdout
 */
putchar(c) {
    syscall(1, 1, &c, charlen(c));
}

/**
 * Writes the given number to stdout, in decimal form
 */
putnum(n) {
    /* Max number of digits for unsigned 64 bit int */
    auto numstack[19], top;
    top = numstack;

    if (n < 0) {
        putchar('-');
        /* FIXME: Change to -n once unaries are supported */
        n = 0 - n;
    } else if (n == 0) {
        putchar('0');
        return;
    }

    while (n != 0) {
        *top = n % 10;
        n = n / 10;
        top = top + 8;
    }

    while (top != numstack) {
        top = top - 8;
        putchar(*top + '0');
    }
}
