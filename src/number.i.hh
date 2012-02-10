#ifndef NUMBER_I_HH
#define NUMBER_I_HH

#ifndef NUMBER_HH
#error "Please include via parent file"
#endif

#include "decl.hh"

// Type mapping
template<>
struct to_type<Number::Type>{
  template<Number::Type t> struct get;
};

template<> template<>
struct to_type<Number::Type>::get<Number::Type::complex>{
  typedef Number::complex_type type;
};

template<> template<>
struct to_type<Number::Type>::get<Number::Type::real>{
  typedef Number::real_type type;
};

template<> template<>
struct to_type<Number::Type>::get<Number::Type::integer>{
  typedef Number::integer_type type;
};


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
  default:
    return 0; // invalid conversion!
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
  default:
    return 0; // invalid conversion!
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
  default:
    return 0; // invalid conversion!
  }
}

#endif //NUMBER_I_HH
