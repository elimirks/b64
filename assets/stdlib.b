/**
 * Returns the length of the given wide char.
 */
charlen(c) {
    auto len 0;
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

/* Writes the first ASCII char of the given wide char to stdout */
putcharSingle(c) {
    syscall(1, 1, &c, 1);
}

/**
 * Writes the given number to stdout, in decimal form
 */
putnum(n) {
    /* Max number of digits for unsigned 64 bit int */
    auto numstack[19], top;
    top = numstack;

    if (n < 0) {
        putcharSingle('-');
        n = -n;
    } else if (n == 0) {
        putcharSingle('0');
        return;
    }

    while (n != 0) {
        *top = n % 10;
        n = n / 10;
        top = top + 8;
    }

    while (top != numstack) {
        top = top - 8;
        putcharSingle(*top + '0');
    }
}
