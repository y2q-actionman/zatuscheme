#include <stdexcept>
#include <utility>
#include <climits>
#include <cassert>

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

