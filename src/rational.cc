#include <stdexcept>
#include <utility>
#include <climits>
#include <cassert>
#include <cmath>
#include <iostream>

#include "rational.hh"
#include "zs_memory.hh"
#include "zs_error.hh"

using namespace std;

Rational& Rational::normalized_reset(long long n, long long d){
  if(d == 0)
    throw std::domain_error("Rational::denominator is 0");

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
    ratio_.n_ = n;
    ratio_.d_ = d;
  }

  return *this;
}

Rational& Rational::operator+=(const Rational& other){
  if(overflow_) return *this;

  auto n = (long long)numerator() * other.denominator()
    + other.numerator() * (long long)denominator();
  auto d = (long long)denominator() * other.denominator();

  return normalized_reset(n, d);
}

Rational& Rational::operator-=(const Rational& other){
  if(overflow_) return *this;

  auto n = (long long)numerator() * other.denominator()
    - other.numerator() * (long long)denominator();
  auto d = (long long)denominator() * other.denominator();

  return normalized_reset(n, d);
}

Rational& Rational::operator*=(const Rational& other){
  if(overflow_) return *this;

  auto n = (long long)numerator() * other.numerator();
  auto d = (long long)denominator() * other.denominator();

  return normalized_reset(n, d);
}

Rational& Rational::operator/=(const Rational& other){
  if(overflow_) return *this;

  auto n = (long long)numerator() * other.denominator();
  auto d = (long long)denominator() * other.numerator();

  return normalized_reset(n, d);
}

Rational& Rational::negate(){
  if(overflow_) return *this;

  return normalized_reset(-(long long)numerator(), (long long)denominator());
}

Rational& Rational::inverse(){
  if(overflow_) return *this;

  return normalized_reset((long long)denominator(), (long long)numerator());
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
    auto frac_p = d - int_p;
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


// utilities
Lisp_ptr wrap_number(const Rational& q){
  if(q.is_convertible<int>()){
    return {Ptr_tag::integer, static_cast<int>(q)};
  }else if(q.is_convertible<Rational>()){
    return {zs_new<Rational>(q)};
  }else{
    cerr << "integer overflow occured. coerced into real.\n";
    return {zs_new<double>(static_cast<double>(q))};
  }
}

Lisp_ptr to_exact(Lisp_ptr p){
  switch(p.tag()){
  case Ptr_tag::integer:
  case Ptr_tag::rational:
    return p;
  case Ptr_tag::real:
    return wrap_number(static_cast<int>(*p.get<double*>()));
  case Ptr_tag::complex:
    throw zs_error("number error: conversion from complex to exact number is not supprted.\n");
  default:
    UNEXP_CONVERSION("exact");
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
  default:
    UNEXP_CONVERSION("inexact");
  }
}
