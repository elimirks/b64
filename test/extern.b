thing() {
    extrn ext;
    return(ext);
}

main() {
    extrn ext;
    ext = 42;
    return(thing());
}

ext;