#include <inttypes.h>
#include <math.h>
#include <stdio.h>
#include <stdlib.h>

// 入力：n <= 4.5*10^18, nは平方数
int64_t isqrt(int64_t n)
{
    // 4.5*10^18 < 2^62
    // 真の答えは 2 <= _ < 2^31 の範囲にある
    int64_t low = 2, high = INT64_C(1) << 31;
    while (low < high) {
        int64_t mid = (low + high) / 2;
        int64_t mid2 = mid * mid;
        if (mid2 < n) {
            low = mid;
        } else if (mid2 == n) {
            return mid;
        } else {
            high = mid;
        }
    }
    return low;
}

struct result {
    int64_t p, q;
};

// 入力：N <= 9*10^18
struct result solve(int64_t N)
{
    // 2080083^3 < 9*10^18 < 2080084^3
    for (int64_t a = 2; a <= 2080083; ++a) {
        if (N % a == 0) {
            int64_t b = N / a;
            if (b % a == 0) {
                // a = p
                return (struct result){.p = a, .q = b / a};
            } else {
                // a = q
                return (struct result){.p = isqrt(b), .q = a};
            }
        }
    }
    abort();
}

int main()
{
    int T;
    scanf("%d", &T);
    for (int i = 0; i < T; ++i) {
        int64_t N;
        scanf("%" SCNd64, &N);
        struct result r = solve(N);
        printf("%" PRId64 " %" PRId64 "\n", r.p, r.q);
    }
}
