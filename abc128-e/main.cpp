#include <vector>
#include <iostream>
#include <tuple>
#include <algorithm>

int main()
{
    using std::get;
    int n, q;
    std::cin >> n >> q;
    // 1 <= n <= 2*10^5, 1 <= q <= 2*10^5
    std::vector<std::tuple<int, int, int>> works(n);
    for (auto& t : works) {
        std::cin >> get<0>(t) >> get<1>(t) >> get<2>(t);
    }
    std::vector<int> ds(q);
    for (auto& d : ds) {
        std::cin >> d;
    }
    std::sort(works.begin(), works.end(), [](std::tuple<int, int, int>& lhs, std::tuple<int, int, int>& rhs) {
            if (get<2>(rhs) < get<2>(lhs)) {
                return true;
            } else if (get<2>(rhs) > get<2>(lhs)) {
                return false;
            } else {
                return lhs < rhs;
            }
        });
    std::vector<int> result(q, -1);
    for (auto& p : works) {
        // 構造化束縛 (C++17) が欲しい
        int s = get<0>(p), t = get<1>(p), x = get<2>(p);
        int s_ = s - x;
        int t_ = t - x;
        // s_ <= *it となるような最初の it を探す
        auto it = std::lower_bound(ds.begin(), ds.end(), s_);
        // t_ <= *it2 となるような最初の it2 を探す
        auto it2 = std::lower_bound(it, ds.end(), t_);
        std::fill(result.begin() + (it - ds.begin()), result.begin() + (it2 - ds.begin()), x);
    }
    for (auto v : result) {
        std::cout << v << std::endl;
    }
}
