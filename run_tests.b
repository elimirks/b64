#import "./assets/stdlib.b";

/* TODO: Simplify once we have more sane stdlib functions for listing dirs */
main() {
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

                /* FIXME: Why does this only break in Github actions? */
                printf("name: %s, %d*n", name, strlen(name));

                auto start,end;
                start = name;
                end = name + 24;

                while (start != end) {
                    printf("%d*n", *start & 255);
                    start =+ 1;
                }

                slash_name = strcat("/", name);
                printf("slash_name: %s, %d*n", slash_name, strlen(slash_name));

                start = slash_name;
                end = slash_name + 24;

                while (start != end) {
                    printf("%d*n", *start & 255);
                    start =+ 1;
                }

                abs_path = strcat(test_dir, slash_name);
                printf("abs_path: %s, %d*n", abs_path, strlen(abs_path));

                print_divider(8 + strlen(abs_path));
                printf("Running %s*n", abs_path);
                print_divider(8 + strlen(abs_path));

                if (system("./target/release/b64", "-r", abs_path, 0) != 0) {
                   panic("Test failed");
                }
                free(abs_path);
                free(slash_name);
            }
            it =+ reclen;
        }
    }
}

print_divider(len) {
    while (len-- > 0) {
        putchar('=');
    }
    putchar('*n');
}