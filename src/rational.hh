#ifndef RATIONAL_HH
#define RATIONAL_HH

class Rational {
public:
  explicit Rational(int);
  Rational(int, int);
  Rational(const Rational&) = default;
  Rational(Rational&&) = default;

  ~Rational() = default;

  Rational& operator=(const Rational&) = default;
  Rational& operator=(Rational&&) = default;

  // allow public access
  int numerator;
  int denominator;
};

// utilities
template<typename T> T gcd(T, T);

#include "rational.i.hh"

#endif // RATIONAL_HH
