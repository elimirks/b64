#import "../assets/stdlib.b";

/* Run with ./target/release/b64 -r examples/args.b -- hello world */
/* Or if you compile this instead of running, you can pass the args normally */
main(argc, argv) {
    printf("argc:    %d*n", argc);
    printf("argv:    %d*n", argv);
    printf("argv[0]: %s*n", argv[0]);
    printf("argv[1]: %s*n", argv[1]);
    printf("argv[2]: %s*n", argv[2]);
}