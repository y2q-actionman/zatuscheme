#ifndef NUMBER_HH
#define NUMBER_HH

#include <complex>
#include <iosfwd>

class Number{
public:
  typedef std::complex<double> complex_type;

  enum class Type {
    uninitialized = -1,
      complex, real,
      integer
  };

private:
  Type type_;

  union{
    complex_type z_;
    double f_;
    long i_;
  };

public:
  Number() :
    type_(Type::uninitialized){}

  explicit Number(const complex_type&);
  explicit Number(double);
  explicit Number(long);

  Number(const Number&) = default;
  Number(Number&&) = default;

  ~Number() = default;

  Number& operator=(const Number&) = default;
  Number& operator=(Number&&) = default;


  Type type() const
  { return type_; };

  template <typename T>
  T get() const;
};

Number parse_number(std::istream&);

Number to_exact(const Number&);
Number to_inexact(const Number&);

#include "number.i.hh"

#endif //NUMBER_HH
