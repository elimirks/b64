thing() {
    extrn ext;
    return(ext);
}

main() {
    extrn ext, vec;
    ext = 42;
    vec[0] = 3;
    vec[1] = 5;
    /* Should return 50 */
    return(thing() + vec[0] + vec[1]);
}

ext;
vec[2];