@import "../assets/stdlib.b";

main() {
    auto x, y;
    putstr("Should be 3: ");
    putnumln(2 / 2 + 2);

    putstr("Should be 3: ");
    putnumln(2 + 2 / 2);

    putstr("Should be 2: ");
    putnumln(8 / (x = y = 2 + 2));

    putstr("Should be 4: ");
    putnumln(x);

    putstr("Should be 5: ");
    putnumln(++y);

    putstr("Should be 5: ");
    putnumln(y);

    x = 5;
    y = 7;
    putstr("Should be 7 7 2:*n");
    putnumln(x =+ y = 2);
    putnumln(x);
    putnumln(y);

    x = 5;
    y = 7;
    putstr("Should be 9 9 9:*n");
    putnumln(x = y =+ 2);
    putnumln(x);
    putnumln(y);
}

putnumln(n) {
    putnum(n);
    putchar('*n');
}