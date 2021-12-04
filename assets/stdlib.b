strcmp(s1, s2) {
    auto i 0, delta;

    /* Widechar ops are like SIMD here lol */
    while (1) {
        /* If one of the strings ends here, handle differently */
        if ((s1[i] & (0377 << 56)) == 0) break;
        if ((s2[i] & (0377 << 56)) == 0) break;
        delta = s2[i] - s1[i];
        if (delta > 0) return(1);
        if (delta < 0) return(-1);
        i =+ 1;
    }
    return(charcmp(s1[i], s2[i]));
}

charcmp(c1, c2) {
    auto i 0, delta;

    while (i < 8) {
        delta = (c2 >> (i*8)) - (c1 >> (i*8));
        if (delta > 0) return(1);
        if (delta < 0) return(-1);
        i =+ 1;
    }
    /* If we haven't returned by now, they're the same */
    return(0);
}

/**
 * Returns the length of the given string reference.
 * The returned length is in _bytes_, not length in wide chars
 */
strlen(s) {
    auto len 0;
    while (1) {
        auto clen;
        clen = charlen(*s);
        len =+ clen;
        if (clen < 8) {
            break;
        }

        s =+ 8;
    }
    return(len);
}

/**
 * Writes the given string reference to stdout.
 */
putstr(s) {
    syscall(1, 1, s, strlen(s));
}

/**
 * Returns the length of the given wide char.
 */
charlen(c) {
    auto len 0;
    while (c) {
        c =>> 8;
        len =+ 1;
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
        n =/ 10;
        top =+ 8;
    }

    while (top != numstack) {
        top =- 8;
        putcharSingle(*top + '0');
    }
}
