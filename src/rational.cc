#include <stdexcept>
#include <utility>
#include <climits>
#include <cassert>
#include <cmath>

#include "rational.hh"

using namespace std;

void Rational::normalized_reset(long long n, long long d){
  if(d == 0)
    throw std::domain_error("Rational::denominator is 0");

  auto gcd_val = gcd(n, d);
  n /= gcd_val;
  d /= gcd_val;

  if(d < 0){
    n = -n;
    d = -d;
  }

  if(n < INT_MIN || n > INT_MAX
     || d < INT_MIN || d > INT_MAX){
    overflow_ = true;
    float_ = (double)n / (double)d;
  }else{
    ratio_.n_ = n;
    ratio_.d_ = d;
  }
}

Rational::Rational(int n, int d)
  : overflow_(false){
  normalized_reset(n, d);
}

Rational::Rational(long long n, long long d)
  : overflow_(false){
  normalized_reset(n, d);
}

Rational::operator int() const{
  assert(is_convertible<int>());
  return numerator();
}

Rational::operator double() const{
  assert(is_convertible<double>());
  if(overflow_){
    return float_;
  }else{
    return static_cast<double>(numerator()) / static_cast<double>(denominator());
  }
}

bool Rational::operator==(const Rational& other) const{
  if(overflow_) return false;

  // assumes rationals are normalized.
  return (numerator() == other.numerator()) && (denominator() == other.denominator());
}

bool Rational::operator<(const Rational& other) const{
  if(overflow_) return false;

  return (numerator() * other.denominator()) < (other.numerator() * denominator());
}

Rational& Rational::operator+=(const Rational& other){
  if(overflow_) return *this;

  auto n = (long long)numerator() * other.denominator()
    + other.numerator() * (long long)denominator();
  auto d = (long long)denominator() * other.denominator();

  normalized_reset(n, d);
  return *this;
}

Rational& Rational::operator-=(const Rational& other){
  if(overflow_) return *this;

  auto n = (long long)numerator() * other.denominator()
    - other.numerator() * (long long)denominator();
  auto d = (long long)denominator() * other.denominator();

  normalized_reset(n, d);
  return *this;
}

Rational& Rational::operator*=(const Rational& other){
  if(overflow_) return *this;

  auto n = (long long)numerator() * other.numerator();
  auto d = (long long)denominator() * other.denominator();

  normalized_reset(n, d);
  return *this;
}

Rational& Rational::operator/=(const Rational& other){
  if(overflow_) return *this;

  auto n = (long long)numerator() * other.denominator();
  auto d = (long long)denominator() * other.numerator();

  normalized_reset(n, d);
  return *this;
}

Rational& Rational::negate(){
  if(overflow_) return *this;

  normalized_reset(-(long long)numerator(), (long long)denominator());
  return *this;
}

Rational& Rational::inverse(){
  if(overflow_) return *this;

  normalized_reset((long long)denominator(), (long long)numerator());
  return *this;
}

Rational rationalize(double answer, double error){
  // from:
  // http://en.wikipedia.org/wiki/Continued_fraction#Infinite_continued_fractions
  double d = answer;

  int h_2 = 0, h_1 = 1;
  int k_2 = 1, k_1 = 0;
  long long h_0;
  long long k_0;

  while(1){
    auto int_p = (long long)floor(d);
    auto frac_p = d - int_p;
    d = 1 / frac_p;

    h_0 = int_p * h_1 + h_2;
    k_0 = int_p * k_1 + k_2;

    if(h_0 < INT_MIN || h_0 > INT_MAX
       || k_0 < INT_MIN || k_0 > INT_MAX){
      // integer overflow
      break;
    }

    auto sub_ans = (double)h_0 / (double)k_0;

    if(abs(sub_ans - answer) <= error){
      break;
    }

    h_2 = h_1;
    h_1 = (int)h_0;
    k_2 = k_1;
    k_1 = (int)k_0;
  }

  return Rational{h_0 , k_0};
}

Rational& Rational::expt(const Rational& other){
  if(other.denominator() != 1){
    overflow_ = true;
    float_ = std::pow(static_cast<double>(*this),
                      static_cast<double>(other));
    return *this;
  }

  const auto base_r = *this;
  normalized_reset(1, 1);

  auto ex = other.numerator();
  for(; ex > 0; --ex){
    operator*=(base_r);
    if(overflow_) break;
  }

  if(overflow_){
    auto base_d = static_cast<double>(base_r);
    for(; ex > 0; --ex) float_ *= base_d;
  }

  return *this;
}
