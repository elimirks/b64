#import "./assets/stdlib.b";

/* TODO: Simplify once we have more sane stdlib functions for listing dirs */
main(argc, argv) {
    if (argc != 2) {
        printf("Usage: %s path/to/b64*n", argv[0]);
        exit(1);
    }

    auto test_dir;
    test_dir = "./test";

    auto fd;
    fd = syscall(2, test_dir, 0, 0);
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
            panic("Failed listing direcotry contents.");
        }

        auto it;
        it = buf;
        while (it - buf < nread) {
            auto name, reclen;
            reclen = *(it + 16) & 0177777;
            name = it + 18;

            if (strcmp(name, ".") != 0 & strcmp(name, "..") != 0) {
                auto abs_path, slash_name;
                /* TODO: Implement sprintf and use it here */
                slash_name = strcat("/", name);
                abs_path = strcat(test_dir, slash_name);

                print_divider(8 + strlen(abs_path));
                printf("Running %s*n", abs_path);
                print_divider(8 + strlen(abs_path));

                if (system(argv[1], "-r", abs_path, 0) != 0) {
                   panic("Test failed");
                }
                free(abs_path);
                free(slash_name);
            }
            it =+ reclen;
        }
    }
    printf("Great success!*n");
}

print_divider(len) {
    while (len-- > 0) {
        putchar('=');
    }
    putchar('*n');
}
