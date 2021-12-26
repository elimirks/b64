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

show_strcat() {
    putstr(strcat("Hello Mars, ", "goodbye Earth*n"));
}

main() {
    show_strcmp();
    show_strcat();
}
