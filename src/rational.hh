#ifndef RATIONAL_HH
#define RATIONAL_HH

#include "lisp_ptr.hh"

class Rational {
public:
  explicit Rational(long long, long long);
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

  Rational& expt(const Rational&);

private:
  bool overflow_;
  union {
    struct {
      int n_;                       // includes sign.
      int d_;
    } ratio_;
    double float_;
  };

  Rational& normalized_reset(long long, long long);
};

Rational rationalize(double, double);

// utilities
template<typename T> T gcd(T, T);

template<typename T> T coerce(Lisp_ptr);

Lisp_ptr wrap_number(int);
Lisp_ptr wrap_number(const Rational&);
Lisp_ptr wrap_number(double);
Lisp_ptr wrap_number(const Complex&);
Lisp_ptr wrap_number(bool);

Lisp_ptr to_exact(Lisp_ptr);
Lisp_ptr to_inexact(Lisp_ptr);

#include "rational.i.hh"

#endif // RATIONAL_HH
