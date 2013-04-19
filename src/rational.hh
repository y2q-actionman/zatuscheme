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
  { return ratio_.n_; }

  int denominator() const
  { return ratio_.d_; }

  template <typename T> bool is_convertible() const;

  operator int() const;
  operator double() const;

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
  bool overflow_;
  union {
    struct {
      int n_;                       // includes sign.
      int d_;
    } ratio_;
    double float_;
  };

  void normalized_reset(long long, long long);
};

// utilities
template<typename T> T gcd(T, T);

#include "rational.i.hh"

#endif // RATIONAL_HH
