#include <inttypes.h>
#include <math.h>
#include <stdio.h>

int main()
{
    // 3037000499^2 < 2^63-1 < 3037000500^2
    for (int64_t i = 0; i <= 3037000499; ++i) {
        int64_t n = i * i;
        int64_t j = (int64_t)sqrt((double)n);
        if (i != j) {
            printf("%" PRId64 "\n", i);
            return 0;
        }
    }
    puts("Done");
}
