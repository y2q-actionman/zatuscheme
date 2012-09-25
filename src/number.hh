#ifndef NUMBER_HH
#define NUMBER_HH

#include <complex>
#include <cstdio>

#include "util.hh"

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
  typename zs::call_traits<T>::type get() const;

  template <typename T> T coerce() const;

  explicit operator bool() const
  { return type_ != Type::uninitialized; }

private:
  Type type_;

  union{
    complex_type z_;
    real_type f_;
    integer_type i_;
  };
};

Number parse_number(FILE*);

Number to_exact(const Number&);
Number to_inexact(const Number&);

bool eqv(const Number&, const Number&);

void print(FILE*, const Number&);

const char* stringify(Number::Type);

#include "number.i.hh"

#endif //NUMBER_HH
