#ifndef RATIONAL_HH
#define RATIONAL_HH

class Rational {
public:
  explicit Rational(int, int denom = 1);
  Rational(const Rational&) = default;
  Rational(Rational&&) = default;

  ~Rational() = default;

  Rational& operator=(const Rational&) = default;
  Rational& operator=(Rational&&) = default;

  explicit operator double() const;

  void normalize();

  // allow public access
  int numerator;                // includes sign.
  int denominator;
};

bool operator==(const Rational&, const Rational&);
bool operator!=(const Rational&, const Rational&);

bool operator<(const Rational&, const Rational&);
bool operator>(const Rational&, const Rational&);
bool operator<=(const Rational&, const Rational&);
bool operator>=(const Rational&, const Rational&);

Rational operator+(const Rational&, const Rational&);
Rational operator-(const Rational&);
Rational operator-(const Rational&, const Rational&);
Rational operator*(const Rational&, const Rational&);
Rational operator/(const Rational&, const Rational&);


// utilities
template<typename T> T gcd(T, T);

#include "rational.i.hh"

#endif // RATIONAL_HH
