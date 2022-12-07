#import "../assets/stdlib.b";

main() {
    auto fd;
    fd = syscall(2, "examples/lazyquine.b", 0, 0);
    if (fd < 0) {
        panic("Failed opening file.");
    }
    auto buf 0;
    auto bufsize 0;
    while (getline(&buf, &bufsize, fd) != -1) {
        printf("%s*n", buf);
    }
    free(buf);
}
