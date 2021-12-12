@import "../assets/stdlib.b";
@import "../assets/memory.b";

main() {
    auto c 10, i 3;
    putnum(i << 3);
    putstr("*n");
    putnum(i * 8);
    putstr("*n");
    putstr("Hello world!*n");

    putstr(num2str(-321));
    putstr("*n");
}