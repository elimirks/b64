#import "../assets/stdlib.b";

main() {
    switch ('b' - 1) {
    case 1:
        putstr("One*n");
        break;
    case 'a':
        putstr("a is for apple*n");
        break;
    default:
        putstr("Something else*n");
    }
}
