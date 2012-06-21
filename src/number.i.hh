#ifndef NUMBER_I_HH
#define NUMBER_I_HH

#ifndef NUMBER_HH
#error "Please include via parent file"
#endif

#include "decl.hh"
#include "util.hh"

// Type mapping
template<Number::Type t, typename T>
T to_type() = delete;

template<>
Number::complex_type to_type<Number::Type::complex>() = delete;

template<>
Number::real_type to_type<Number::Type::real>() = delete;

template<>
Number::integer_type to_type<Number::Type::integer>() = delete;


template<>
inline constexpr
Number::Type to_tag<Number::Type, Number::complex_type>(){
  return Number::Type::complex;
}

template<>
inline constexpr
Number::Type to_tag<Number::Type, Number::real_type>(){
  return Number::Type::real;
}

template<>
inline constexpr
Number::Type to_tag<Number::Type, Number::integer_type>(){
  return Number::Type::integer;
}


// Number class definitions
inline
Number::Number(const complex_type& c) :
  type_(Type::complex), z_(c){}

inline
Number::Number(complex_type::value_type r, complex_type::value_type i) :
  type_(Type::complex), z_(r, i){}

inline
Number::Number(real_type d) :
  type_(Type::real), f_(d){}

inline
Number::Number(integer_type i) :
  type_(Type::integer), i_(i){}


template <>
inline
Number::complex_type Number::get() const{
  switch(type_){
  case Type::complex:
    return z_;
  case Type::real:
    return complex_type{f_};
  case Type::integer:
    return complex_type{static_cast<real_type>(i_)};
  case Type::uninitialized:
  default:
    UNEXP_CONVERSION("complex");
  }
}

template <>
inline
Number::real_type Number::get() const{
  switch(type_){
  case Type::real:
    return f_;
  case Type::integer:
    return static_cast<real_type>(i_);
  case Type::complex:
  case Type::uninitialized:
  default:
    UNEXP_CONVERSION("real");
  }
}

template <>
inline
Number::integer_type Number::get() const{
  switch(type_){
  case Type::integer:
    return i_;
  case Type::complex:
  case Type::real:
  case Type::uninitialized:
  default:
    UNEXP_CONVERSION("integer");
  }
}

#endif //NUMBER_I_HH
