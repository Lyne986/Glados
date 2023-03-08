long longEvenSum(long nb) {
    long res = 0;
    long max = nb;

    for (long i = 0; i <= max; i++) {
        if (i % 2 == 0) {
            res += i;
        }
    }
    return res;
}