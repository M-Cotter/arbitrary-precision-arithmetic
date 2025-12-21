#include<iostream>
#include<vector>
#include<climits>
#include<algorithm>
#include<bit>
#include<cstdint>
#include<string>

using u64 = unsigned long long;
using u128 = unsigned __int128;

bool isOver(u64 a, u64 b) {
    return a > ULLONG_MAX - b;
}

u64 doCarry(u64 a, u64 b, u64 carry) {
    if ((a > ULLONG_MAX - b) || (carry == 1 && a + b + 1 == 0)) return 1;
    return 0;
}

u64 mulCarry(u64 a, u64 b) {
    return (u64)((u128)a * (u128)b >> 64);
}

// Unsigned arbitrary length integer
class BigInt {
    // Stores digits base u64
    std::vector<u64> digits;

    public:
        // Constructor from input a digit vector {ones, u64, u64^2, ...}
        BigInt(const std::vector<u64>& x) : digits(x) {
            while (digits.size() > 1 && digits.back() == 0) {
                    digits.pop_back();
            }
            if (digits.empty()) {
                digits.push_back(0);
            }
        }

        // Constructor from a string
        BigInt(std::string x) {
            digits = {0};

            for (char c : x) {
                *this = (*this).small_mul(10);
                *this += BigInt(std::vector<u64>{(u64)(c-'0')});
            }
        }

        void print() const {
            for (u64 i : digits) {
                std::cout << i << " ";
            }
        }

        std::string base_ten() {
            BigInt ten_power(std::vector<u64>{1});
            std::string output = "";

            while (ten_power <= *this) {
                output.insert(output.begin(), char('0' + ((*this / ten_power) % BigInt(std::vector<u64>{10})).digits.at(0)));
                ten_power = ten_power.small_mul(10);
            }

            return output;
        }

        void print_bt() {
            std::cout << base_ten();
        }

        // Comparison operators
        bool operator==(const BigInt &o) const {
             return o.digits == digits;
        }

        bool operator!=(const BigInt &o) const {
            if (digits == o.digits) {
                return false;
            }
            return true;
        }

        bool operator>(const BigInt &o) const {
            if (digits.size() != o.digits.size()) {
                return digits.size() > o.digits.size();
            }

            for (unsigned int i = digits.size(); i-- > 0;) {
                if (digits[i] != o.digits[i]) {
                    return digits[i] > o.digits[i];
                }
            }
                return false;
        }

        bool operator<(const BigInt &o) const {
            if (digits.size() != o.digits.size()) {
                return digits.size() < o.digits.size();
            }

            for (unsigned int i = digits.size(); i-- > 0;) {
                if (digits[i] != o.digits[i]) {
                    return digits[i] < o.digits[i];
                }
            }
                return false;
        }

        bool operator>=(const BigInt &o) const {
            return (*this > o) || (o == *this);
        }

        bool operator<=(const BigInt &o) const {
            return (*this < o) || (o == *this);
        }

        //Sum
        BigInt operator+(const BigInt &o) const {
            unsigned int mSize = std::max(digits.size(), o.digits.size());
            std::vector<u64> sumDigits(mSize, 0);

            u64 carry = 0;
            for (unsigned int i = 0; i < mSize; i++) {
                u64 a = (i < digits.size()) ? digits[i] : 0;
                u64 b = (i < o.digits.size()) ? o.digits[i] : 0;

                u64 sum = a + b + carry;
                carry = doCarry(a, b, carry);
                sumDigits[i] = sum;
            }

            sumDigits[mSize] = carry;

            return BigInt(sumDigits);
        }

        BigInt operator-(const BigInt &o) const {
            if (*this < o) throw std::runtime_error("Subtracted number must be less than the first");

            std::vector<u64> diffDigits = digits;
            
            for (unsigned int i = 0; i < o.digits.size(); i++) {
                if (digits[i] < o.digits[i]) {
                    diffDigits[i + 1] -= 1;
                }
                diffDigits[i] = diffDigits[i] - o.digits[i];
            }
            return BigInt(diffDigits);
        }

        BigInt &operator+=(const BigInt &b) {
            *this = *this + b;
            return *this;
        }

        BigInt &operator-=(const BigInt &b) {
            *this = *this - b;
            return *this;
        }

        // Multiplication of a BigInt by u64
        BigInt small_mul(u64 n) const {
            BigInt prodSum(std::vector<u64>{});
            for (unsigned int i = 0; i < digits.size(); i++) {
                std::vector<u64> summand(i, 0);
                summand.push_back(digits.at(i) * n);
                summand.push_back(mulCarry(digits.at(i), n));
                prodSum += BigInt(summand);
            }
            return prodSum;
        }

        // Add zeros to the end of a BigInt (i.e. multiply it by (ULLONG_MAX + 1)^n)
        BigInt addZeros(int n) const {
            std::vector<u64> zeros(n, 0);
            zeros.insert(zeros.end(), digits.begin(), digits.end());
            return BigInt(zeros);
        }

        BigInt shift(int n) const {
            return small_mul(1ULL << n);
        }

        BigInt remLargeDigits(int n) const {
            return BigInt(std::vector<u64>(digits.begin(), digits.end() - n));
        }

        u64 digit(unsigned int n) {
            if (n < digits.size()) {
                return digits.at(n);
            } else {
                return 0;
            }
        }

        BigInt operator*(const BigInt &o) const {
            BigInt prodSum(std::vector<u64>{});
            for (unsigned int i = 0; i < digits.size(); i++) {
                prodSum += o.small_mul(digits.at(i)).addZeros(i);
            }
            return prodSum;
        }

        BigInt &operator*=(const BigInt &b) {
            *this = *this * b;
            return *this;
        }

        u64 normalizer(u64 x) const {
            if (x == 0) {
                throw std::runtime_error("cannot normalize zero");
            }
            return (u64)1 << std::countl_zero(x);
        }

        u64 single_div(const BigInt divisor) {

            if (divisor.digits.size() != digits.size() - 1) {
                throw std::runtime_error("Divisor is the wrong size");
            }
            if (divisor.addZeros(1) < *this) {
                throw std::runtime_error("Divisor is too small");
            }
            if (divisor.digits.back() < ULLONG_MAX/2 - 2) {
                throw std::runtime_error("First digit of divisor is too small");
            }

            u64 qh = (u64)((((u128)digits.back() << 64) + (u128)digits.at(digits.size() - 2)) / (u128)divisor.digits.back());
            
            while (divisor.small_mul(qh) > *this) {
                qh -= 1;
            }
            
            return qh;
        }

        BigInt single_div_fd(const BigInt divisor) {
            u64 fd = largest_digits(divisor.digits.size() + 1).single_div(divisor);
            return BigInt(std::vector<u64>{fd}).addZeros(digits.size() - divisor.digits.size() - 1);
        }

        BigInt largest_digits(int n) {
            return BigInt(std::vector<u64>(digits.end() - n, digits.end()));
        }

        
        BigInt operator/(const BigInt &o) const {
            if (o == BigInt(std::vector<u64>{})) {
                throw std::runtime_error("Division by zero");
            }

            BigInt quotient(std::vector<u64>{});

            u64 d = normalizer(o.digits.back());
            BigInt v = o.small_mul(d);
            BigInt u = small_mul(d);

            while (u >= v) {
                if (v < u.largest_digits(v.digits.size())) {
                    quotient += BigInt(std::vector<u64>{1}).addZeros(u.digits.size() - v.digits.size());
                    u -= v.addZeros(u.digits.size() - v.digits.size());
                }

                if (v > u) break;

                quotient += u.single_div_fd(v);
                u -= u.single_div_fd(v) * v;
            }


            return quotient;
        }

        BigInt operator%(const BigInt &o) {
            return *this - (*this / o) * o;
        }

        BigInt operator^(unsigned int n) const {
            BigInt power(std::vector<u64>{1});
            BigInt square = *this;

            while (n > 0) {
                if (n & 1) {
                    power *= square;
                }

                square *= square;
                n >>= 1;
            }

            return power;
        }

};

int main() {
    BigInt b(std::vector<u64>{ULLONG_MAX,0,10,11});
    BigInt c(std::vector<u64>{10,11});
    (b + c).print();
    return 0;
}

