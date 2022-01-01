#import "memory.b";

/*
 * Compares 2 strings. Return negative if "s1 < s2"
 */
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

/*
 * Compares 2 chars. Return negative if "c1 < c2"
 */
charcmp(c1, c2) {
    auto i 0, delta;
    while (i < 64) {
        delta = (c2 >> i) - (c1 >> i);
        if (delta > 0) return(1);
        if (delta < 0) return(-1);
        i =+ 8;
    }
    /* If we haven't returned by now, they're the same */
    return(0);
}

/*
 * Concatenates the two given strings.
 * @return a malloc'd new string
 */
strcat(s1, s2) {
    auto s1len, s2len, s2lastIndex, newLen, newQuadLen, new, newptr;
    s1len = strlen(s1);
    s2len = strlen(s2);
    /* +1 in case we need it for a null byte.
     * Since we round up for newQuadLen, this
     * won't ever needlessly allocate an extra quad.
     */
    newLen = s1len + s2len + 1;
    /* Allocate number of quads we need */
    newQuadLen = ((newLen + 7) & ~7) >> 3;
    new = malloc(newQuadLen);
    memmove(new, s1, ((s1len + 7) & ~7) >> 3);

    s2lastIndex = ((s2len + 7) & ~7) >> 3;
    newptr = new + s1len;
    memmove(newptr, s2, s2lastIndex);

    /* Must manually null terminate if s2len is a multiple of 8 */
    if ((s2len & 255) == 0) {
       new[newQuadLen - 1] = 0;
    }
    return(new);
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
_putcharSingle(c) {
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
        _putcharSingle('-');
        n = -n;
    } else if (n == 0) {
        _putcharSingle('0');
        return;
    }

    while (n != 0) {
        *top = n % 10;
        n =/ 10;
        top =+ 8;
    }

    while (top != numstack) {
        top =- 8;
        _putcharSingle(*top + '0');
    }
}

/**
 * Writes the given number to stdout, in octal form.
 * Outputs UNSIGNED octal integers.
 */
putnumoct(n) {
    /* Max number of digits for unsigned 64 bit octal number */
    /* 1777777777777777777777 */
    auto numstack[22], top;
    top = numstack;

    if (n == 0) {
        _putcharSingle('0');
        return;
    }

    while (n != 0) {
        *top = n & 7;
        n =>> 3;
        top =+ 8;
    }

    while (top != numstack) {
        top =- 8;
        _putcharSingle(*top + '0');
    }
}

/*
 * Allocates a new string of the given number represented in decimal
 */
num2str(n) {
    /* At most 19 indexes required, for the string "-9223372036854775808" */
    auto charStack[19], stackTop;
    stackTop = charStack;

    auto str, len 0;
    /* 3 quads = 24 bytes, enough to store the longest signed quad value */
    str = malloc(3);
    str[0] = 0;
    str[1] = 0;
    str[2] = 0;

    if (n < 0) {
        str[0] = '-';
        len = 1;
        n = -n;
    } else if (n == 0) {
        str[0] = '0';
        return(str);
    }

    while (n != 0) {
        *stackTop = (n % 10) + '0';
        n =/ 10;
        stackTop =+ 8;
    }

    while (charStack != stackTop) {
        auto charIndex, strIndex;
        strIndex = len / 8;
        charIndex = len % 8;

        len =+ 1;
        stackTop =- 8;

        putchar(*stackTop);
        putchar(',');
        putnum(charIndex);
        putchar('*n');

        str[strIndex] =| *stackTop << (charIndex << 3);
    }
    return(str);
}

exit(code) {
    syscall(60, code);
}

panic(message) {
    putchar('PANIC: ');
    putstr(message);
    putchar('*n');
    exit(1);
}

/*
 * Prints the given format string based on the args.
 * Since B doesn't support vargs, we're capped at 10 format parameters.
 * Using anything more than 10 will panic the program.
 * Supported format tokens:
 * - %d for signed decimal
 * - %o for unsigned octal 
 * - %c for wide character
 * - %s for string
 * - %% for a percent symbol (useful if you want to print a literal '%d')
 * - % followed by anything else will print out the character verbatim
 */
printf(fmt, a0, a1, a2, a3, a4, a5, a6, a7, a8, a9) {
    auto fmt_ptr, is_perc 0, arg_num 0, arg;
    fmt_ptr = fmt;

    /* Iterate over each char, not quad */
    while ((*fmt_ptr & 0377) != 0) {
        auto c;
        c = *fmt_ptr & 0377;

        if (is_perc) {
            switch (arg_num) {
            case 0: arg = a0; break;
            case 1: arg = a1; break;
            case 2: arg = a2; break;
            case 3: arg = a3; break;
            case 4: arg = a4; break;
            case 5: arg = a5; break;
            case 6: arg = a6; break;
            case 7: arg = a7; break;
            case 8: arg = a8; break;
            case 9: arg = a9; break;
            default:
                panic("Too many %'s in printf!");
            }
            is_perc = 0;

            if (c == 'd') {
                arg_num =+ 1;
                putnum(arg);
            } else if (c == 'o') {
                arg_num =+ 1;
                putnumoct(arg);
            } else if (c == 's') {
                arg_num =+ 1;
                putstr(arg);
            } else if (c == 'c') {
                arg_num =+ 1;
                putchar(arg);
            } else if (c == '%') {
                putchar(c);
            } else {
                putchar('%');
                putchar(c);
            }
        } else {
            if (c == '%' & arg_num < 10) {
                is_perc = 1;
            } else {
                putchar(c);
            }
        }
        fmt_ptr =+ 1;
    }
}

/* arg0 is implicitly set to filename
 * IMPORTANT: You MUST null terminate the args if you use less than 9
 */
system(filename, a1, a2, a3, a4, a5, a6, a7, a8, a9) {
    auto env 0;
    auto args[10];
    args[0] = filename;
    args[1] = a1;
    args[2] = a2;
    args[3] = a3;
    args[4] = a4;
    args[5] = a5;
    args[6] = a6;
    args[7] = a7;
    args[8] = a8;
    args[9] = a9;
    args[10] = 0;

    auto pid;
    if ((pid = sys_fork()) == 0) {
        /* Child process */
        sys_execve(filename, args, &env);
    } else {
        sys_wait4(pid, 0, 0, 0);
    }
}

sys_fork() {
    return(syscall(57));
}

sys_execve(filename, argv, envp) {
    syscall(59, filename, argv, envp);
}

sys_wait4(upid, stat_addr, options, rusage) {
    return(syscall(61, upid, stat_addr, options, rusage));
}