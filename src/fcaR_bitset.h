#ifndef FCAR_BITSET_H
#define FCAR_BITSET_H

#include <vector>
#include <cstdint>
#include <algorithm>
#include <cstring>

#ifdef _MSC_VER
#include <intrin.h>
#define FCAR_CTZLL _tzcnt_u64
#else
#define FCAR_CTZLL __builtin_ctzll
#endif

namespace fcaR {

class FastBitset {
public:
    static const size_t npos = static_cast<size_t>(-1);

public:
    std::vector<uint64_t> words;
    int n_bits;
    int n_words;

public:
    FastBitset() : n_bits(0), n_words(0) {}

    void init(int n) {
        n_bits = n;
        n_words = (n + 63) >> 6;
        words.assign(n_words, 0);
    }

    inline void clear() {
        std::fill(words.begin(), words.end(), 0ULL);
    }

    inline void set() {
        std::fill(words.begin(), words.end(), ~0ULL);
        int rem = n_bits & 63;
        if (rem > 0 && n_words > 0) words[n_words - 1] &= (1ULL << rem) - 1;
    }

    inline void set(int i) {
        if (i >= 0 && i < n_bits) words[i >> 6] |= (1ULL << (i & 63));
    }

    inline void reset(int i) {
        if (i >= 0 && i < n_bits) words[i >> 6] &= ~(1ULL << (i & 63));
    }

    inline bool test(int i) const {
        if (i < 0 || i >= n_bits) return false;
        return (words[i >> 6] & (1ULL << (i & 63))) != 0;
    }

    inline bool any() const {
        for (uint64_t w : words) if (w != 0) return true;
        return false;
    }

    inline bool none() const { return !any(); }

    inline int count() const {
        int c = 0;
        for (uint64_t w : words) c += __builtin_popcountll(w);
        return c;
    }

    inline size_t find_first() const {
        for (int i = 0; i < n_words; ++i) {
            if (words[i] != 0) return (static_cast<size_t>(i) << 6) + FCAR_CTZLL(words[i]);
        }
        return npos;
    }

    inline size_t find_next(size_t last) const {
        if (last == npos) return find_first();
        int word_idx = static_cast<int>(last >> 6);
        int bit_idx = static_cast<int>(last & 63);
        if (word_idx >= n_words) return npos;

        uint64_t mask = 0;
        if (bit_idx < 63) mask = words[word_idx] & (~0ULL << (bit_idx + 1));
        
        if (mask != 0) return (static_cast<size_t>(word_idx) << 6) + FCAR_CTZLL(mask);

        for (int i = word_idx + 1; i < n_words; ++i) {
            if (words[i] != 0) return (static_cast<size_t>(i) << 6) + FCAR_CTZLL(words[i]);
        }
        return npos;
    }

    inline FastBitset& operator&=(const FastBitset& other) {
        int min_w = std::min(n_words, other.n_words);
        for (int i = 0; i < min_w; ++i) words[i] &= other.words[i];
        for (int i = min_w; i < n_words; ++i) words[i] = 0;
        return *this;
    }

    inline FastBitset& operator|=(const FastBitset& other) {
        int min_w = std::min(n_words, other.n_words);
        for (int i = 0; i < min_w; ++i) words[i] |= other.words[i];
        return *this;
    }

    inline void bitwise_and_not(const FastBitset& other) {
        int min_w = std::min(n_words, other.n_words);
        for (int i = 0; i < min_w; ++i) words[i] &= ~other.words[i];
    }

    inline bool is_subset_of(const FastBitset& other) const {
        int min_w = std::min(n_words, other.n_words);
        for (int i = 0; i < min_w; ++i) {
            if ((words[i] & ~other.words[i]) != 0) return false;
        }
        for (int i = min_w; i < n_words; ++i) {
            if (words[i] != 0) return false;
        }
        return true;
    }

    inline bool operator==(const FastBitset& other) const {
        if (n_bits != other.n_bits) return false;
        for (int i = 0; i < n_words; ++i) {
            if (words[i] != other.words[i]) return false;
        }
        return true;
    }

    inline bool operator!=(const FastBitset& other) const { return !(*this == other); }

    inline bool operator<(const FastBitset& other) const {
        if (n_bits != other.n_bits) return n_bits < other.n_bits;
        for (int i = n_words - 1; i >= 0; --i) {
            if (words[i] < other.words[i]) return true;
            if (words[i] > other.words[i]) return false;
        }
        return false;
    }

    inline FastBitset operator~() const {
        FastBitset res = *this;
        for (int i = 0; i < n_words; ++i) res.words[i] = ~res.words[i];
        int rem = n_bits & 63;
        if (rem > 0 && n_words > 0) res.words[n_words - 1] &= (1ULL << rem) - 1;
        return res;
    }

    inline FastBitset operator|(const FastBitset& other) const {
        FastBitset res = *this;
        res |= other;
        return res;
    }

    inline FastBitset operator&(const FastBitset& other) const {
        FastBitset res = *this;
        res &= other;
        return res;
    }
};

} // namespace fcaR

#endif
