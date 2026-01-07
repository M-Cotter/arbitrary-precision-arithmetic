#include<bits/stdc++.h>
#include<iostream>
#include<vector>
#include<climits>
#include<bits/stdc++.h>
#include<string>

using u64 = uint64_t;
using u128 = unsigned __int128;

// Removes leading zeros from an input vector
void normalize(std::vector<u64> &v) {
    while (!v.empty() && (v.back() == 0)) {
        v.pop_back();
    }
}

// Prints each digit (64 bit chunk) base 10
void print_digits(std::vector<u64> digits) {
    for (u64 i : digits) {
        std::cout << i << " ";
    }

    std::cout << "\n";
}

// Compares the magnitudes of vectors, (-1 : <), (0 : =), (1 : >)
int compare(std::vector<u64> a, std::vector<u64> b) {
    if (a.size() < b.size()) return -1;
    if (a.size() > b.size()) return 1;
    
    for (int i = a.size() - 1; i >= 0; i--) {
        if (a[i] < b[i]) return -1;
        if (a[i] > b[i]) return 1;
    }

    return 0;
}

// Bitwise operations

// This computes the lg of a vector of u64s, which will be used later for division 
unsigned int log_two(std::vector<u64> v) {
    if (v.empty()) return 0;

    return (unsigned int)((v.size() - 1) * 64 + (64 - __builtin_clzll(v.back())));
}

// Shift the vector left, aka small_mul it efficiently by 2^shift.
std::vector<u64> shift_left(std::vector<u64> &v, unsigned int shift) {
    if (v.empty()) return {};
    unsigned int limb_shift = shift / 64, bit_shift = shift % 64, n = v.size();
    std::vector<u64> res(n + limb_shift + (bit_shift ? 1 : 0), 0);

    u64 carry = 0;
    for (size_t i = 0; i < n; ++i) {
        u128 cur = (u128)v[i] << bit_shift;
        cur |= carry;
        res[i + limb_shift] = (u64)cur;
        carry = (u64)(cur >> 64);
    }

    if (bit_shift) {
        res[n + limb_shift] = carry;
    }

    normalize(res);
    return res;
}

// Makes a specific bit one in a vector of u64s, defined by pos.
void make_bit_one(std::vector<u64> &v, unsigned int pos) {
    unsigned int digit = pos / 64, bit = pos % 64;
    if (v.size() <= digit) v.resize(digit + 1, 0);

    v[digit] |= (u64(1) << bit);
}


// Arithmetic operations

// Adds two vectors in place
void add(std::vector<u64> &digits, std::vector<u64> other) {
    unsigned int mSize = std::max(digits.size(), other.size());
    digits.resize(mSize, 0);

    u64 carry = 0, a, b, sum;
    for (unsigned int i = 0; i < mSize; i++) {
        a = digits[i];
        b = (i < other.size()) ? other[i] : 0;

        sum = digits[i] + ((i < other.size()) ? other[i] : 0) + carry;

        carry = ((a > ULLONG_MAX - b) || (carry == 1 && a + b + carry == 0)) ? 1 : 0;
        digits[i] = sum;
    }

    if (carry == 1) digits.push_back(1);
}


// Subtracts two u64 vectors in place. Bad stuff (idk) will happen if you try to subtract a number greater than the first.
void sub(std::vector<u64> &digits, std::vector<u64> other) {
    u128 borrow = 0;
    unsigned int n = digits.size();
    unsigned int m = other.size();
    
    for (size_t i = 0; i < n; i++) {
        u128 di = digits[i];
        u128 oi = (i < m) ? other[i] : 0;
        u128 temp = di;
        u128 subt = oi + borrow;
        
        if (temp >= subt) {
            digits[i] = (u64)(temp - subt);
            borrow = 0;
        } else {
            digits[i] = (u64)((((u128)1 << 64) + temp) - subt);
            borrow = 1;
        }
    }

    normalize(digits);
}

// Small mul for multiplying a vector by u64 (in place)
void small_mul(std::vector<u64> &digits, u64 n) {
    if (n == 0) {
        digits = {0};
        return;
    } else if (n == 1) {
        return;
    }

    u128 carry = 0, interProd;

    for (unsigned int i = 0; i < digits.size(); i++) {
        interProd = (u128)digits[i] * n + carry;
        digits[i] = (u64)interProd;
        carry = interProd >> 64;
    }

    if (carry != 0) digits.push_back((u64)carry);
}


std::vector<u64> small_mul_op(std::vector<u64> digits, u64 n) {
    small_mul(digits, n);
    return digits;
}

// Multiplcation (not in place)
std::vector<u64> mul(std::vector<u64> digits, std::vector<u64> other) {
    std::vector<u64> prodSum(digits.size() + other.size(), 0);

    u128 carry = 0, interProd;

    for(unsigned int i = 0; i < digits.size(); i++) {
        for (unsigned int j = 0; j < other.size(); j++) {
            interProd = (u128)other[j] * digits[i] + carry;
            prodSum[i + j] += (u64)interProd;
            carry = interProd >> 64;
        }
        if (carry != 0) prodSum[i + other.size()] += (u64)carry;
    }

    normalize(prodSum);
    return prodSum;
}

// Small division, not in place, for dividing a vector by u64
std::vector<u64> small_div(std::vector<u64> digits, u64 n) {
    if (n == 0) throw std::runtime_error("small_div by 0");

    std::vector<u64> quotient(digits.size(), 0);
    u64 interQuot;

    for (int i = digits.size() - 1; i >= 0; i--) {
        interQuot = (u64)((((u128) ((i == (int)digits.size() - 1) ? 0 : digits[i + 1]) << 64) | digits[i]) / n);
        quotient[i] = interQuot;
        digits[i] -= interQuot * n;
    }

    normalize(quotient);
    return quotient;
}

u64 small_mod(std::vector<u64> digits, u64 n) {
    if (n == 0) throw std::runtime_error("small_mod by 0");

    std::vector<u64> quotient(digits.size(), 0);
    u64 interQuot;

    for (int i = digits.size() - 1; i >= 0; i--) {
        interQuot = (u64)((((u128) ((i == (int)digits.size() - 1) ? 0 : digits[i + 1]) << 64) | digits[i]) / n);
        quotient[i] = interQuot;
        digits[i] -= interQuot * n;
    }

    return digits[0];
}



// Division :) (also not in place)
std::vector<u64> div(std::vector<u64> digits, std::vector<u64> other) {
    if (other.empty()) throw std::runtime_error("div by zero");

    if (compare(digits, other) < 0) {
        return std::vector<u64>{0};
    }

    unsigned int ltd = log_two(digits), lto = log_two(other), shift = ltd - lto;

    std::vector<u64> quotient;
    quotient.reserve( (shift / 64) + 2 );

    std::vector<u64> otherShift;
    
    while (compare(digits, other) >= 0) {
        ltd = log_two(digits);
        lto = log_two(other);
        shift = ltd - lto;
        otherShift = shift_left(other, shift);
        
        if (compare(otherShift, digits) > 0) {
            shift -= 1;
            otherShift = shift_left(other, shift);
        }

        sub(digits, otherShift);
        make_bit_one(quotient, shift);
        normalize(digits);
    }

    normalize(quotient);
    normalize(digits);
    return quotient;
}

// Modulo (also not in place)
std::vector<u64> mod(std::vector<u64> digits, std::vector<u64> other) {
    if (other.empty()) throw std::runtime_error("div by zero");

    if (compare(digits, other) < 0) {
        return digits;
    }

    unsigned int ltd = log_two(digits), lto = log_two(other), shift = ltd - lto;

    std::vector<u64> quotient;
    quotient.reserve( (shift / 64) + 2 );

    std::vector<u64> otherShift;

    while (compare(digits, other) >= 0) {
        ltd = log_two(digits);
        lto = log_two(other);
        shift = ltd - lto;
        otherShift = shift_left(other, shift);

        if (compare(otherShift, digits) > 0) {
            shift -= 1;
            otherShift = shift_left(other, shift);
        }

        sub(digits, otherShift);
        make_bit_one(quotient, shift);
        normalize(digits);
    }

    normalize(quotient);
    normalize(digits);
    return digits;
}

std::pair<std::vector<u64>, std::vector<u64>> div_mod(std::vector<u64> digits, std::vector<u64> other) {
    if (other.empty()) throw std::runtime_error("div by zero");

    if (compare(digits, other) < 0) {
        return {std::vector<u64>{0}, digits};
    }

    unsigned int ltd = log_two(digits), lto = log_two(other), shift = ltd - lto;

    std::vector<u64> quotient;
    quotient.reserve( (shift / 64) + 2 );

    std::vector<u64> otherShift;

    while (compare(digits, other) >= 0) {
        ltd = log_two(digits);
        lto = log_two(other);
        shift = ltd - lto;
        otherShift = shift_left(other, shift);

        if (compare(otherShift, digits) > 0) {
            shift -= 1;
            otherShift = shift_left(other, shift);
        }

        sub(digits, otherShift);
        make_bit_one(quotient, shift);
        normalize(digits);
    }

    normalize(quotient);
    normalize(digits);
    return {quotient,digits};
}

std::vector<u64> power(std::vector<u64> digits, unsigned int n) {
    std::vector<u64> power(std::vector<u64>{1});
    while (n > 0) {
        if (n & 1) {
            power = mul(digits, power);
        }
        digits = mul(digits, digits);
        n >>= 1;
    }
    
    return power;
}

std::string base_ten(std::vector<u64> digits) {
    if (digits.empty()) return "0";

    std::vector<u64> ten = {10};
    std::string out;

    while (digits[0] != 0) {
        auto [q, r] = div_mod(digits, ten);
        u64 digit = r.empty() ? 0 : r[0];
        out.push_back(char('0' + digit));
        digits = q;
    }

    std::reverse(out.begin(), out.end());
    return out;
}



int main() {
    std::vector<u64> u0 = {7};
    std::vector<u64> v0 = power(u0, 1000);
    std::cout << base_ten(v0);
}

