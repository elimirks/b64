#import "../assets/stdlib.b";

main() {
    system("/bin/echo", "Hello from", "child process...", 0);
    printf("Hello from parent process!*n");
}