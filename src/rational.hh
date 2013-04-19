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

  int numerator() const
  { return n_; }

  int denominator() const
  { return d_; }

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

private:
  int n_;                       // includes sign.
  int d_;

  template<typename T> void normalized_reset(T, T);
};

// utilities
template<typename T> T gcd(T, T);

#include "rational.i.hh"

#endif // RATIONAL_HH
