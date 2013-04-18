#include <stdexcept>

#include "rational.hh"

using namespace std;

Rational::Rational(int n, int d)
  : numerator(n), denominator(d){
  if(d == 0)
    throw std::domain_error("Rational::denominator is 0");

  normalize();
}
  
Rational::operator double() const{
  return static_cast<double>(numerator)
    / static_cast<double>(denominator);
}

void Rational::normalize(){
  auto gcd_val = gcd(numerator, denominator);
  numerator /= gcd_val;
  denominator /= gcd_val;

  if(denominator < 0){
    numerator = -numerator;
    denominator = -denominator;
  }
}

bool operator==(const Rational& r1, const Rational& r2){
  // assumes rationals are normalized.
  return (r1.numerator == r2.numerator)
    && (r1.denominator == r2.denominator);
}

bool operator<(const Rational& r1, const Rational& r2){
  return (r1.numerator * r2.denominator) < (r2.numerator * r1.denominator);
}

Rational operator+(const Rational& r1, const Rational& r2){
  return Rational(r1.numerator * r2.denominator + r2.numerator * r1.denominator,
                  r1.denominator * r2.denominator);
}

Rational operator-(const Rational& r1){
  return Rational(-r1.numerator, r1.denominator);
}

Rational operator-(const Rational& r1, const Rational& r2){
  return Rational(r1.numerator * r2.denominator - r2.numerator * r1.denominator,
                  r1.denominator * r2.denominator);
}

Rational operator*(const Rational& r1, const Rational& r2){
  return Rational(r1.numerator * r2.numerator,
                  r1.denominator * r2.denominator);
}

Rational operator/(const Rational& r1, const Rational& r2){
  return Rational(r1.numerator * r2.denominator,
                  r1.denominator * r2.numerator);
}
