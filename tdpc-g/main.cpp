#include <iostream>
#include <vector>
#include <map>
#include <cstdint>
#include <utility>
#include <string>
#include <algorithm>
#include <limits>

using Integer = std::uint64_t;

struct AlphabetSet {
    std::int32_t val;
    static const AlphabetSet empty;
    constexpr auto elem(char c) const -> bool
    {
        return !!(val & (1 << (c - 'a')));
    }
    constexpr auto insert(char c) const -> AlphabetSet
    {
        return {val | (1 << (c - 'a'))};
    }
};
const AlphabetSet AlphabetSet::empty = {0};

auto allOccurrenceNotIn(const char *s, AlphabetSet e) -> std::vector<std::pair<char, const char *>>
{
    std::vector<std::pair<char, const char *>> res;
    for (; *s; ++s) {
        if (!e.elem(*s)) {
            res.emplace_back(*s, s+1);
            e = e.insert(*s);
        }
    }
    return res;
}

const char *strBase;
std::vector<std::map<int, Integer>> memo;

auto numberOfSubstringsX(AlphabetSet const e, const char *s) -> Integer
{
    if (*s == '\0') {
        return 1;
    }
    auto& memoMap = memo.at(s - strBase);
    if (memoMap.count(e.val) > 0) {
        return memoMap[e.val];
    }
    Integer val = 0;
    if (e.elem(*s)) {
        val = numberOfSubstringsX(e, s+1);
    } else {
        auto a = numberOfSubstringsX(AlphabetSet::empty, s+1);
        auto b = numberOfSubstringsX(e.insert(*s), s+1);
        auto c = a + b;
        if (c < a || c < b) {
            val = std::numeric_limits<std::uint64_t>::max();
        } else {
            val = c;
        }
    }
    memoMap[e.val] = val;
    return val;
}

auto lexIndexX(Integer i, AlphabetSet const e, const char *s) -> std::string
{
    if (i == 0) {
        return "";
    } else {
        auto const& t = allOccurrenceNotIn(s, e);
        if (t.empty()) {
            return "Eel";
        } else {
            auto const& p = *std::min_element(t.begin(), t.end(), [](std::pair<char, const char *> const& a, std::pair<char, const char *> const& b) { return a.first < b.first; });
            auto n = numberOfSubstringsX(AlphabetSet::empty, p.second);
            if (i <= n) {
                return p.first + lexIndexX(i - 1, AlphabetSet::empty, p.second);
            } else {
                return lexIndexX(i - n, e.insert(p.first), s);
            }
        }
    }
}

int main()
{
    std::string str;
    std::getline(std::cin, str);
    Integer k = 0;
    std::cin >> k;
    memo.resize(str.size() + 1);
    strBase = str.c_str();
    std::cout << lexIndexX(k, AlphabetSet::empty, str.c_str()) << std::endl;
}
