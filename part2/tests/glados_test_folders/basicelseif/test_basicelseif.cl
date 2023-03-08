int add(int a, int b) {
    if (b == 0) {
        return a;
    } else if (b > 0) {
        return b + a;
    } else {
        return b - a;
    }
}
