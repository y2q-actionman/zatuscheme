#ifndef NUMBER_I_HH
#define NUMBER_I_HH

#ifndef NUMBER_HH
#error "Please include via parent file"
#endif

#include <cassert>

inline
Number::Number(const complex_type& c) :
  type_(Type::complex), z_(c){}

inline
Number::Number(double d) :
  type_(Type::real), f_(d){}

inline
Number::Number(long i) :
  type_(Type::integer), i_(i){}


template <>
Number::complex_type Number::get() const{
  switch(type_){
  case Type::complex:
    return z_;
  case Type::real:
    return complex_type{f_};
  case Type::integer:
    return complex_type{static_cast<double>(i_)};
  default:
    throw "invalid conversion!";
  }
}

template <>
double Number::get() const{
  switch(type_){
  case Type::real:
    return f_;
  case Type::integer:
    return static_cast<double>(i_);
  case Type::complex:
  default:
    throw "invalid conversion!";
  }
}

template <>
long Number::get() const{
  switch(type_){
  case Type::integer:
    return i_;
  case Type::complex:
  case Type::real:
  default:
    throw "invalid conversion!";
  }
}

#endif //NUMBER_I_HH
