#include <stdexcept>
#include <utility>

#include "rational.hh"

using namespace std;

   // TODO: overflow check
template<typename T>
void Rational::normalized_reset(T n, T d){
  if(d == 0)
    throw std::domain_error("Rational::denominator is 0");

  auto gcd_val = gcd(n, d);
  n /= gcd_val;
  d /= gcd_val;

  if(d < 0){
    n = -n;
    d = -d;
  }

  numerator = n;
  denominator = d;
}

Rational::Rational(int n, int d)
  : numerator(), denominator(){
  normalized_reset(n, d);
}
  
Rational::operator double() const{
  return static_cast<double>(numerator)
    / static_cast<double>(denominator);
}

bool Rational::operator==(const Rational& other) const{
  // assumes rationals are normalized.
  return (numerator == other.numerator)
    && (denominator == other.denominator);
}

bool Rational::operator<(const Rational& other) const{
  return (numerator * other.denominator) < (numerator * other.denominator);
}

Rational& Rational::operator+=(const Rational& other){
  auto n = (long long)numerator * other.denominator
    + other.numerator * (long long)denominator;
  auto d = (long long)denominator * other.denominator;

  normalized_reset(n, d);
  return *this;
}

Rational& Rational::operator-=(const Rational& other){
  auto n = (long long)numerator * other.denominator
    - other.numerator * (long long)denominator;
  auto d = (long long)denominator * other.denominator;

  normalized_reset(n, d);
  return *this;
}

Rational& Rational::operator*=(const Rational& other){
  auto n = (long long)numerator * other.numerator;
  auto d = (long long)denominator * other.denominator;

  normalized_reset(n, d);
  return *this;
}

Rational& Rational::operator/=(const Rational& other){
  auto n = (long long)numerator * other.denominator;
  auto d = (long long)denominator * other.numerator;

  normalized_reset(n, d);
  return *this;
}

Rational& Rational::negate(){
  normalized_reset(-(long long)numerator, (long long)denominator);
  return *this;
}

Rational& Rational::inverse(){
  normalized_reset((long long)denominator, (long long)numerator);
  return *this;
}

