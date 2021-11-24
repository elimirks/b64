/* Run with "test/hello.b assets/stdlib.b" */
main() {
    auto hello ' Hello ';
    extrn world;
    putnum(charlen(world));
    putchar(hello);
    putchar(world);
}

world 'world!*n';