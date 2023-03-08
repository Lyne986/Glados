int basicWhile(int m) {
    int a = m;
    int nbLoop = 0;
    while (a >= 0) {
        a = a - 1;
        nbLoop = nbLoop + 1;
    }
    return nbLoop;
}