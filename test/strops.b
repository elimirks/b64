#import "../assets/stdlib.b";

show_strcmp() {
    putstr("strcmp:*n");
    putnum(strcmp("aaaaaaaaac", "aaaaaaaaab"));
    putstr("*n");
    putnum(strcmp("", "eouotheusntaoeusntaehutsnahtoneuah"));
    putstr("*n");
    putnum(strcmp("a", "e"));
    putstr("*n");
    putnum(strcmp("a", "eouotheusntaoeusntaehutsnahtoneuah"));
    putstr("*n");
    putnum(strcmp("z", "e"));
    putstr("*n");
    putnum(strcmp("z", "eouotheusntaoeusntaehutsnahtoneuah"));
    putstr("*n");
}

main() {
    show_strcmp();
}
