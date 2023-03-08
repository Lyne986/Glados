int evenSum(int nb) {
    int sum = 0;
    int a = 0;
    int i = 0;

    while (i <= nb) {
        if (i % 2 == 0) {
            sum = sum + i;
        }
        i = i + 1;
    }
    return sum;
}