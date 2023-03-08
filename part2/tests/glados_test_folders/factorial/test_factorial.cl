int fact(int a) {
    if (a < 2) {
        return 1;
    }
    return a * fact(a - 1);
}
