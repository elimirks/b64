abs(n) {
    if (n < 0) return(-n);
    return(n);
}

signum(n) {
    if (n < 0) return(-1);
    if (n > 0) return(1);
    return(0);
}
