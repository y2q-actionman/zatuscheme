#include <cassert>
#include <climits>
#include <cmath>

#include "rational.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

static_assert(sizeof(int) < sizeof(long long),
              "integer overflow cannot be treated properly");

using namespace std;

// TODO: make this as constructor.
Rational& Rational::normalized_reset(long long n, long long d){
  if(d == 0)
    throw_zs_error({}, "Rational::denominator is 0");

  auto gcd_val = gcd(n, d);
  n /= gcd_val;
  d /= gcd_val;

  if(d < 0){
    n = -n;
    d = -d;
  }

  if(n < INT_MIN || n > INT_MAX
     || d < INT_MIN || d > INT_MAX){
    overflow_ = true;
    float_ = (double)n / (double)d;
  }else{
    ratio_.n_ = static_cast<int>(n);
    ratio_.d_ = static_cast<int>(d);
  }

  return *this;
}

Rational::operator double() const{
  assert(is_convertible<double>());
  if(overflow_){
    return float_;
  }else{
    return static_cast<double>(numerator()) / static_cast<double>(denominator());
  }
}

Rational& Rational::expt(const Rational& other){
  if(other.denominator() != 1){
    overflow_ = true;
    float_ = std::pow(static_cast<double>(*this),
                      static_cast<double>(other));
    return *this;
  }

  const auto base_r = *this;
  normalized_reset(1, 1);

  auto ex = other.numerator();
  for(; ex > 0; --ex){
    operator*=(base_r);
    if(overflow_) break;
  }

  if(overflow_){
    auto base_d = static_cast<double>(base_r);
    for(; ex > 0; --ex) float_ *= base_d;
  }

  return *this;
}


Rational rationalize(double answer, double error){
  // from:
  // http://en.wikipedia.org/wiki/Continued_fraction#Infinite_continued_fractions
  double d = answer;

  int h_2 = 0, h_1 = 1;
  int k_2 = 1, k_1 = 0;
  long long h_0;
  long long k_0;

  while(1){
    auto int_p = (long long)floor(d);
    auto frac_p = d - floor(d);
    d = 1 / frac_p;

    h_0 = int_p * h_1 + h_2;
    k_0 = int_p * k_1 + k_2;

    if(h_0 < INT_MIN || h_0 > INT_MAX
       || k_0 < INT_MIN || k_0 > INT_MAX){
      // integer overflow
      break;
    }

    auto sub_ans = (double)h_0 / (double)k_0;

    if(abs(sub_ans - answer) <= error){
      break;
    }

    h_2 = h_1;
    h_1 = (int)h_0;
    k_2 = k_1;
    k_1 = (int)k_0;
  }

  return Rational{h_0 , k_0};
}

// utilities
template<>
int coerce(Lisp_ptr p){
  if(p.tag() == Ptr_tag::integer){
    return p.get<int>();
  }else{
    UNEXP_DEFAULT();
  }
}

template<>
Rational coerce(Lisp_ptr p){
  if(p.tag() == Ptr_tag::rational){
    return *p.get<Rational*>();
  }else{
    return Rational(coerce<int>(p));
  }
}

template<>
double coerce(Lisp_ptr p){
  if(p.tag() == Ptr_tag::real){
    return *(p.get<double*>());
  }else{
    return static_cast<double>(coerce<Rational>(p));
  }
}

template<>
Complex coerce(Lisp_ptr p){
  if(p.tag() == Ptr_tag::complex){
    return *(p.get<Complex*>());
  }else{
    return Complex(coerce<double>(p), 0);
  }
}

bool is_numeric_type(Lisp_ptr p){
  switch(p.tag()){
  case Ptr_tag::integer:
  case Ptr_tag::rational:
  case Ptr_tag::real:
  case Ptr_tag::complex:
    return true;
  case Ptr_tag::undefined: case Ptr_tag::boolean:
  case Ptr_tag::character: case Ptr_tag::cons:
  case Ptr_tag::symbol:
  case Ptr_tag::i_procedure: case Ptr_tag::n_procedure:
  case Ptr_tag::continuation: case Ptr_tag::syntax_rules:
  case Ptr_tag::string:    case Ptr_tag::vector:
  case Ptr_tag::input_port: case Ptr_tag::output_port:
  case Ptr_tag::env:  case Ptr_tag::syntactic_closure:
  case Ptr_tag::vm_op: case Ptr_tag::vm_argcount:
  case Ptr_tag::notation:
  default:
    return false;
  }
}

bool is_numeric_convertible(Lisp_ptr p, Ptr_tag tag){
  switch(p.tag()){
  case Ptr_tag::integer:
    if(tag == Ptr_tag::integer) return true;
    // fall through
  case Ptr_tag::rational:
    if(tag == Ptr_tag::rational) return true;
    // fall through
  case Ptr_tag::real:
    if(tag == Ptr_tag::real) return true;
    // fall through
  case Ptr_tag::complex:
    return (tag == Ptr_tag::complex);
  case Ptr_tag::undefined: case Ptr_tag::boolean:
  case Ptr_tag::character: case Ptr_tag::cons:
  case Ptr_tag::symbol:
  case Ptr_tag::i_procedure: case Ptr_tag::n_procedure:
  case Ptr_tag::continuation: case Ptr_tag::syntax_rules:
  case Ptr_tag::string:    case Ptr_tag::vector:
  case Ptr_tag::input_port: case Ptr_tag::output_port:
  case Ptr_tag::env:  case Ptr_tag::syntactic_closure:
  case Ptr_tag::vm_op: case Ptr_tag::vm_argcount:
  case Ptr_tag::notation:
  default:
    return false;
  }
}

Lisp_ptr wrap_number(const Rational& q){
  if(q.is_convertible<int>()){
    return Lisp_ptr{static_cast<int>(q)};
  }else if(q.is_convertible<Rational>()){
    return {zs_new<Rational>(q)};
  }else{
    print_zs_warning("integer overflow occured. coerced into real.");
    return {zs_new<double>(static_cast<double>(q))};
  }
}

Lisp_ptr wrap_number(double d){
  return {zs_new<double>(d)};
}

Lisp_ptr wrap_number(const Complex& z){
  return {zs_new<Complex>(z)};
}

Lisp_ptr to_exact(Lisp_ptr p){
  switch(p.tag()){
  case Ptr_tag::integer:
  case Ptr_tag::rational:
    return p;
  case Ptr_tag::real:
    return wrap_number(static_cast<int>(*p.get<double*>()));
  case Ptr_tag::complex:
    throw_zs_error(p, "number error: conversion from complex to exact number is not supprted.\n");
  case Ptr_tag::undefined:
  case Ptr_tag::boolean:
  case Ptr_tag::character:
  case Ptr_tag::cons:
  case Ptr_tag::symbol:
  case Ptr_tag::i_procedure:
  case Ptr_tag::n_procedure:
  case Ptr_tag::continuation:
  case Ptr_tag::string:
  case Ptr_tag::vector:
  case Ptr_tag::input_port:
  case Ptr_tag::output_port:
  case Ptr_tag::env:
  case Ptr_tag::syntactic_closure:
  case Ptr_tag::syntax_rules:
  case Ptr_tag::vm_op:
  case Ptr_tag::vm_argcount:
  case Ptr_tag::notation:
  default:
    UNEXP_DEFAULT();
  }
}

Lisp_ptr to_inexact(Lisp_ptr p){
  switch(p.tag()){
  case Ptr_tag::integer:
  case Ptr_tag::rational:
    return wrap_number(coerce<double>(p));
  case Ptr_tag::real:
  case Ptr_tag::complex:
    return p;
  case Ptr_tag::undefined:
  case Ptr_tag::boolean:
  case Ptr_tag::character:
  case Ptr_tag::cons:
  case Ptr_tag::symbol:
  case Ptr_tag::i_procedure:
  case Ptr_tag::n_procedure:
  case Ptr_tag::continuation:
  case Ptr_tag::string:
  case Ptr_tag::vector:
  case Ptr_tag::input_port:
  case Ptr_tag::output_port:
  case Ptr_tag::env:
  case Ptr_tag::syntactic_closure:
  case Ptr_tag::syntax_rules:
  case Ptr_tag::vm_op:
  case Ptr_tag::vm_argcount:
  case Ptr_tag::notation:
  default:
    UNEXP_DEFAULT();
  }
}
