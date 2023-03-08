int add(int a, int b) {
    return a + b;
}

int evenSum(int nb) {
    int sum = 0;
    int a = 0;

    for (int i = 0; i <= nb; i = i + 1) {
        if (i % 2 == 0) {
            sum = add(sum, i);
        }
    }
    return sum;
}