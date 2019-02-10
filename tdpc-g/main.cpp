#include <iostream>
#include <vector>
#include <cstdint>
#include <utility>
#include <string>
#include <algorithm>
#include <limits>
#include <array>

std::string str;
std::vector<std::array<std::int32_t, 26>> strIndexTable;

// strIndexTable[i][x - 'a'] : 文字列の i 番目以降に最初に現れる文字 x のインデックス（現れない場合は -1）
// となるような2次元配列 strIndexTable を初期化する。
void initStringIndexTable()
{
    strIndexTable.resize(str.size() + 1);
    for(std::size_t j = 0; j < 26; ++j) {
        strIndexTable.back()[j] = -1;
    }
    for(std::size_t i = str.size() - 1; ; --i) {
        strIndexTable.at(i) = strIndexTable.at(i + 1);
        char x = str.at(i);
        strIndexTable.at(i).at(x - 'a') = static_cast<std::int32_t>(i);
        if (i == 0) {
            break;
        }
    }
}

using Pair = std::pair<char, int>;

// 文字列の i 番目以降に出現する文字と、その最初の出現位置の組 Pair(c, j) のリストを返す。
// リストの順番は、アルファベットの若い順である。
auto allOccurrences(int i) -> std::vector<Pair>
{
    std::vector<Pair> res;
    auto const& tbl = strIndexTable.at(i);
    if (i < str.size()) {
        for (int k = 0; k < 26; ++k) {
            if (tbl.at(k) != -1) {
                res.emplace_back('a' + k, tbl.at(k));
            }
        }
    }
    return res;
}

std::vector<std::int64_t> numberOfSubstringsVec;

// numberOfSubstringsVec[i] = 〈文字列の i 番目以降からなるスライス〉の部分文字列の個数
// となるような配列 numberOfSubstringsVec を初期化する。
// ただし、値が十分大きい場合の計算は適宜打ち切られる（十分大きい値が適当に入っている）。
void initNumberOfSubstringsVec()
{
    numberOfSubstringsVec.resize(str.size() + 1);
    const std::int64_t maxI = 1e18 + 1; // 十分大きな値
    for (std::size_t i = str.size(); ; --i) {
        std::int64_t acc = 1;
        for (auto const& p : allOccurrences(i)) {
            if (acc > maxI) {
                // 値が十分大きくなった場合は計算を打ち切ってオーバーフローを回避する
                break;
            }
            auto y = numberOfSubstringsVec.at(p.second + 1);
            if (y > maxI) {
                // 値が十分大きくなった場合はry
                acc = y;
                break;
            }
            acc += y;
        }
        numberOfSubstringsVec.at(i) = acc;
        if (i == 0) {
            break;
        }
    }
}

auto lexIndexSlice(int j, std::int64_t i) -> std::string;

// 与えられた集合 S の要素の部分文字列のうち、辞書順で i 番目となるものを返す。
// S は、文脈で与えられる文字列に対して (最初の一文字, その出現位置) という形の要素からなるリストで、アルファベットの若い順に並んでいる。
// i が大きいときは Nothing を返す。
auto lexSearch(std::int64_t i, std::vector<Pair>::const_iterator it, std::vector<Pair>::const_iterator end) -> std::string
{
    for (;;) {
        if (i == 0) {
            return "";
        } else if (it == end) {
            return "Eel";
        } else {
            auto n = numberOfSubstringsVec.at(it->second + 1);
            if (i <= n) {
                return it->first + lexIndexSlice(it->second + 1, i - 1);
            } else {
                // return lexSearch(i - n, ++it, end);
                i -= n;
                ++it;
            }
        }
    }
}

// 〈文字列の j 番目以降からなるスライス〉の部分文字列のうち、辞書順で i 番目となるものを返す。
// i が大きいときは Nothing を返す。
auto lexIndexSlice(int j, std::int64_t i) -> std::string
{
    auto t = allOccurrences(j);
    return lexSearch(i, t.begin(), t.end());
}

int main()
{
    std::getline(std::cin, str);
    std::int64_t k = 0;
    std::cin >> k;
    initStringIndexTable();
    initNumberOfSubstringsVec();
    std::cout << lexIndexSlice(0, k) << std::endl;
}
