#import "../assets/best.b";

thing() {
    extrn ext;
    return(ext);
}

main() {
    extrn ext, vec;
    ext = 42;
    vec[0] = 3;
    vec[1] = 5;
    assert_eq_int(50, thing() + vec[0] + vec[1]);
}

ext;
vec[] 3, 5;
