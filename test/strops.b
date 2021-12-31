#import "../assets/stdlib.b";

show_strcmp() {
    printf("strcmp:*n");
    printf("%d*n", strcmp("aaaaaaaaac", "aaaaaaaaab"));
    printf("%d*n", strcmp("", "eouotheusntaoeusntaehutsnahtoneuah"));
    printf("%d*n", strcmp("a", "e"));
    printf("%d*n", strcmp("a", "eouotheusntaoeusntaehutsnahtoneuah"));
    printf("%d*n", strcmp("z", "e"));
    printf("%d*n", strcmp("z", "eouotheusntaoeusntaehutsnahtoneuah"));
}

show_strcat() {
    putstr(strcat("Hello Mars, ", "goodbye Earth*n"));
}

main() {
    show_strcmp();
    show_strcat();
}
