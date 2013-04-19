#ifndef RATIONAL_HH
#define RATIONAL_HH

class Rational {
public:
  Rational(int, int);
  Rational(const Rational&) = default;
  Rational(Rational&&) = default;

  ~Rational() = default;

  Rational& operator=(const Rational&) = default;
  Rational& operator=(Rational&&) = default;

  explicit operator double() const;

  bool operator==(const Rational&) const;
  bool operator!=(const Rational&) const;

  bool operator<(const Rational&) const;
  bool operator>(const Rational&) const;
  bool operator<=(const Rational&) const;
  bool operator>=(const Rational&) const;

  Rational& operator+=(const Rational&);
  Rational& operator-=(const Rational&);
  Rational& operator*=(const Rational&);
  Rational& operator/=(const Rational&);

  Rational& negate();
  Rational& inverse();

  // allow public access
  int numerator;                // includes sign.
  int denominator;

private:
  template<typename T> void normalized_reset(T, T);
};

// utilities
template<typename T> T gcd(T, T);

#include "rational.i.hh"

#endif // RATIONAL_HH
