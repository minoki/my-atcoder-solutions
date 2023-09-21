#include <stdbool.h>
#include <stdlib.h>
#include <stdio.h>
int main(void)
{
    int n;
    scanf("%d", &n);
    unsigned long long *input = calloc(n, sizeof(unsigned long long));
    for (int i = 0; i < n; ++i) {
        scanf("%llu", &input[i]);
    }
    unsigned long long p = 1;
    bool overflow = false;
    for (int i = 0; i < n; ++i) {
        unsigned long long a = p, b = input[i];
        if (b == 0ull) {
            puts("0");
            return 0;
        }
        unsigned long long c = a * b;
        if (c > 1000000000000000000ull || c / a != b || c / b != a) {
            overflow = true;
        } else {
            p = c;
        }
    }
    if (overflow) {
        puts("-1");
    } else {
        printf("%llu\n", p);
    }
}
