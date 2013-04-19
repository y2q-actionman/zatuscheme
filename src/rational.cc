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

  this->n_ = n;
  this->d_ = d;
}

Rational::Rational(int n, int d)
  : n_(), d_(){
  normalized_reset(n, d);
}

Rational::operator double() const{
  return static_cast<double>(n_) / static_cast<double>(d_);
}

bool Rational::operator==(const Rational& other) const{
  // assumes rationals are normalized.
  return (n_ == other.n_) && (d_ == other.d_);
}

bool Rational::operator<(const Rational& other) const{
  return (n_ * other.d_) < (other.n_ * d_);
}

Rational& Rational::operator+=(const Rational& other){
  auto n = (long long)n_ * other.d_ + other.n_ * (long long)d_;
  auto d = (long long)d_ * other.d_;

  normalized_reset(n, d);
  return *this;
}

Rational& Rational::operator-=(const Rational& other){
  auto n = (long long)n_ * other.d_ - other.n_ * (long long)d_;
  auto d = (long long)d_ * other.d_;

  normalized_reset(n, d);
  return *this;
}

Rational& Rational::operator*=(const Rational& other){
  auto n = (long long)n_ * other.n_;
  auto d = (long long)d_ * other.d_;

  normalized_reset(n, d);
  return *this;
}

Rational& Rational::operator/=(const Rational& other){
  auto n = (long long)n_ * other.d_;
  auto d = (long long)d_ * other.n_;

  normalized_reset(n, d);
  return *this;
}

Rational& Rational::negate(){
  normalized_reset(-(long long)n_, (long long)d_);
  return *this;
}

Rational& Rational::inverse(){
  normalized_reset((long long)d_, (long long)n_);
  return *this;
}

