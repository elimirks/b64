#import "../assets/stdlib.b";

/*
 * Lists the given directory
 * @see https://man7.org/linux/man-pages/man2/getdents.2.html for reference
 */
main(argc, argv) {
    auto to_list;
    if (argc == 1) {
        to_list = ".";
    } else {
        to_list = argv[1];
    }

    auto fd;
    fd = syscall(2, to_list, 0, 0);
    if (fd == -1) {
        panic("Failed opening directory.");
    }

    /* 1024 bytes of buffer space */
    auto buf[127];

    while (1) {
        auto nread;
        nread = syscall(78, fd, buf, 1024);
        if (nread == 0) {
            break;
        } else if (nread < 0) {
            panic("Failed listing directory contents.");
        }

        auto it;
        it = buf;
        while (it - buf < nread) {
            auto name, reclen;
            reclen = *(it + 16) & 0177777;
            name = it + 18;
            /* printf("reclen: %d, name len: %d, name: %s*n", reclen, strlen(name), name); */
            printf("%s*n", name);
            it =+ reclen;
        }
    }
}
