#include <iostream>
#include <vector>
#include <cstdint>
#include <utility>
#include <string>
#include <algorithm>
#include <limits>

using INT = std::uint64_t;

struct AlphabetSet {
    std::int32_t val;
    static const AlphabetSet empty;
    static const AlphabetSet all;
    constexpr auto elem(char c) const -> bool
    {
        return !!(val & (1 << (c - 'a')));
    }
    constexpr auto insert(char c) const -> AlphabetSet
    {
        return {val | (1 << (c - 'a'))};
    }
    constexpr auto operator==(AlphabetSet rhs) const -> bool
    {
        return val == rhs.val;
    }
    constexpr auto operator!=(AlphabetSet rhs) const -> bool
    {
        return val != rhs.val;
    }
};
const AlphabetSet AlphabetSet::empty = {0};
const AlphabetSet AlphabetSet::all = {(1 << 26) - 1};

auto allOccurrencesNotIn(const char *s, AlphabetSet e) -> std::vector<std::pair<char, const char *>>
{
    std::vector<std::pair<char, const char *>> res;
    for (; *s && e != AlphabetSet::all; ++s) {
        if (!e.elem(*s)) {
            res.emplace_back(*s, s+1);
            e = e.insert(*s);
        }
    }
    return res;
}

const char *strBase;
std::vector<INT> memo;

using Pair = std::pair<char, const char *>;

auto lexIndexX(INT i, const char *s) -> std::string;

auto lexIndexW(INT i, std::vector<Pair>::const_iterator it, std::vector<Pair>::const_iterator end, INT maxI) -> std::string
{
    for (;;) {
        if (i == 0) {
            return "";
        } else if (it == end) {
            return "Eel";
        } else {
            auto n = memo.at(it->second - strBase);
            if (i <= n) {
                return it->first + lexIndexX(i - 1, it->second);
            } else {
                // return lexIndexW(i - n, ++it, end, maxI);
                i -= n;
                ++it;
            }
        }
    }
}

auto lexIndexX(INT i, const char *s) -> std::string
{
    auto t = allOccurrencesNotIn(s, AlphabetSet::empty);
    std::sort(t.begin(), t.end(), [](Pair const& a, Pair const& b) { return a.first < b.first; });
    return lexIndexW(i, t.begin(), t.end(), i);
}

int main()
{
    std::string str;
    // std::ifstream ins("testinput.txt");
    auto& ins = std::cin;
    std::getline(ins, str);
    INT k = 0;
    ins >> k;
    strBase = str.c_str();
    {
        memo.resize(str.size() + 1);
        for (const char *s = strBase + str.size(); /* s >= strBase */; --s) {
            INT val = 1;
            {
                AlphabetSet e = AlphabetSet::empty;
                for (const char *t = s; *t && e != AlphabetSet::all; ++t) {
                    if (!e.elem(*t)) {
                        val += memo.at(t+1 - strBase);
                        if (val > k) {
                            break;
                        }
                        e = e.insert(*t);
                    }
                }
            }
            /*
              auto const& t = allOccurrencesNotIn(s, AlphabetSet::empty);
              for (auto const& u : t) {
              val += memo.at(u.second - strBase);
              if (val > k) {
              break;
              }
              }
            */
            memo.at(s - strBase) = val;
            if (s == strBase) {
                break;
            }
        }
    }
    std::cout << lexIndexX(k, str.c_str()) << std::endl;
}
