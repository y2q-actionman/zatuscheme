#ifndef NUMBER_HH
#define NUMBER_HH

#include <complex>
#include <iosfwd>
#include <cstdio>

class Number{
public:
  typedef std::complex<double> complex_type;
  typedef double real_type;
  typedef long integer_type;

  enum class Type {
    uninitialized = 0,
      complex,
      real,
      integer
  };


  constexpr Number():
    type_(Type::uninitialized){}

  explicit Number(const complex_type&);
  explicit Number(real_type);
  explicit Number(integer_type);

  Number(const Number&) = default;
  Number(Number&&) = default;

  ~Number() = default;

  Number& operator=(const Number&) = default;
  Number& operator=(Number&&) = default;


  Type type() const
  { return type_; };

  template <typename T>
  T get() const;

private:
  Type type_;

  union{
    complex_type z_;
    real_type f_;
    integer_type i_;
  };
};

Number parse_number(std::istream&);

Number to_exact(const Number&);
Number to_inexact(const Number&);

void describe(FILE*, Number::Type);
void describe(FILE*, const Number&);

#include "number.i.hh"

#endif //NUMBER_HH
