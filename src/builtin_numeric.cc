#include <iterator>
#include <vector>
#include <functional>
#include <algorithm>
#include <numeric>
#include <cstdlib>
#include <cmath>
#include <limits>
#include <iostream>
#include <sstream>

#include "builtin_numeric.hh"
#include "vm.hh"
#include "builtin.hh"
#include "zs_error.hh"
#include "number.hh"
#include "lisp_ptr.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "printer.hh"
#include "equality.hh"

using namespace std;

bool is_numeric_type(Lisp_ptr p){
  auto tag = p.tag();
  return (tag == Ptr_tag::integer
          || tag == Ptr_tag::real
          || tag == Ptr_tag::complex);
}

namespace {

bool is_integer_type(Lisp_ptr p){
  return (p.tag() == Ptr_tag::integer);
}

bool is_rational_type(Lisp_ptr p){
  return (false) // TODO: add rational!
    || is_integer_type(p);
}

bool is_real_type(Lisp_ptr p){
  return (p.tag() == Ptr_tag::real)
    || is_rational_type(p);
}

bool is_complex_type(Lisp_ptr p){
  return (p.tag() == Ptr_tag::complex)
    || is_real_type(p);
}

template<typename T> T coerce(Lisp_ptr);

template<>
int coerce(Lisp_ptr p){
  if(p.tag() == Ptr_tag::integer){
    return p.get<int>();
  }else{
    UNEXP_DEFAULT();
  }
}

template<>
double coerce(Lisp_ptr p){
  if(p.tag() == Ptr_tag::real){
    return *(p.get<double*>());
  }else{
    return static_cast<double>(coerce<int>(p));
  }
}

template<>
std::complex<double> coerce(Lisp_ptr p){
  if(p.tag() == Ptr_tag::complex){
    return *(p.get<complex<double>*>());
  }else{
    return complex<double>(coerce<double>(p), 0);
  }
}


zs_error number_type_check_failed(const char* func_name, Lisp_ptr p){
  return zs_error_arg1(func_name,
                       printf_string("arg is not %s!", stringify(Ptr_tag::number)),
                       {p});
}

template<typename Fun>
inline Lisp_ptr number_pred(Fun&& fun){
  ZsArgs args;

  if(is_numeric_type(args[0])){
    return Lisp_ptr{fun(args[0])};
  }else{
    return Lisp_ptr{false};
  }
}


struct complex_found{
  static constexpr const char* msg = "native func: number compare: complex cannot be ordinated\n";
  bool operator()(const Number::complex_type&, const Number::complex_type&) const{
    throw zs_error(msg);
  }
};

template<template <typename> class Fun,
         class ComplexComparator = complex_found>
struct number_comparator {
  inline bool operator()(Lisp_ptr p1, Lisp_ptr p2){
    if(is_integer_type(p1) && is_integer_type(p2)){
      static const Fun<int> fun;
      return fun(coerce<int>(p1), coerce<int>(p2));
    }else if(is_real_type(p1) && is_real_type(p2)){
      static const Fun<double> fun;
      return fun(coerce<double>(p1), coerce<double>(p2));
    }else if(is_complex_type(p1) && is_complex_type(p2)){
      static const ComplexComparator fun;
      return fun(coerce<std::complex<double> >(p1),
                 coerce<std::complex<double> >(p2));
    }else{
      UNEXP_DEFAULT();
    }
  }
};


template<typename Fun>
inline Lisp_ptr number_compare(const char* name, Fun&& fun){
  ZsArgs args;

  auto i1 = begin(args);
  const auto e = end(args);

  if(!is_numeric_type(*i1)){
    throw number_type_check_failed(name, *i1);
  }
                              
  for(auto i2 = next(i1); i2 != e; i1 = i2, ++i2){
    if(!is_numeric_type(*i2)){
      throw number_type_check_failed(name, *i2);
    }

    if(!fun(*i1, *i2)){
      return Lisp_ptr{false};
    }
  }

  return Lisp_ptr{true};
}


template<template <typename> class Fun>
struct pos_neg_pred{
  inline bool operator()(Lisp_ptr p){
    if(is_integer_type(p)){
      static const Fun<int> fun;
      return fun(coerce<int>(p), 0);
    }else if(is_real_type(p)){
      static const Fun<double> fun;
      return fun(coerce<double>(p), 0);
    }else if(is_complex_type(p)){
      static constexpr Fun<double> fun;
      auto c = coerce<complex<double> >(p);
      return (c.imag() == 0) && fun(c.real(), 0);
    }else{
      UNEXP_DEFAULT();
    }
  }
};


template<template <typename> class Fun>
struct even_odd_pred{
  inline bool operator()(Lisp_ptr p){
    if(is_integer_type(p)){
      static const Fun<int> fun;
      return fun(coerce<int>(p), 0);
    }else{
      return false;
    }
  }
};


template<typename Fun, typename Iter>
inline
Lisp_ptr number_accumulate(const char* name, Lisp_ptr init, const Fun& fun,
                           const Iter& args_begin, const Iter& args_end){
  auto acc = init;

  for(auto i = args_begin, e = args_end; i != e; ++i){
    if(!is_numeric_type(*i)){
      throw number_type_check_failed(name, *i);
    }

    acc = fun(acc, *i);
    if(!acc){ // assumed 'uncaught_exception' context
      return {};
    }
  }

  return acc;
}

template<class Fun>
inline
Lisp_ptr number_accumulate(const char* name, Lisp_ptr init, const Fun& fun){
  ZsArgs args;
  return number_accumulate(name, init, fun, args.begin(), args.end());
}


template<template <typename> class Cmp>
struct minmax_accum{
  inline Lisp_ptr operator()(Lisp_ptr n1, Lisp_ptr n2) const{
    if(!is_numeric_type(n1)){
      throw number_type_check_failed("min/max", n1);
    }

    if(!is_numeric_type(n2)){
      throw number_type_check_failed("mix/max", n2);
    }

    if(is_integer_type(n1) && is_integer_type(n2)){
      static const Cmp<int> cmp;
      if(cmp(coerce<int>(n1), coerce<int>(n2))){
        return n1;
      }else{
        return n2;
      }
    }else if(is_real_type(n1) && is_real_type(n2)){
      static const Cmp<double> cmp;
      if(cmp(coerce<double>(n1), coerce<double>(n2))){
        return n1;
      }else{
        return n2;
      }
    }else if(is_complex_type(n1) && is_complex_type(n2)){
      throw zs_error(complex_found::msg);
    }else{
      UNEXP_DEFAULT();
    }
  }
};    

template<template <typename> class Op>
struct binary_accum{
  inline Lisp_ptr operator()(Lisp_ptr n1, Lisp_ptr n2) const{
    if(!is_numeric_type(n1)){
      throw number_type_check_failed("(binary numeric)", n1);
    }

    if(!is_numeric_type(n2)){
      throw number_type_check_failed("(binary numeric)", n2);
    }


    if(is_integer_type(n1) && is_integer_type(n2)){
      // TODO: add limit check
      static constexpr Op<int> op;
      return Lisp_ptr{Ptr_tag::integer, op(coerce<int>(n1), coerce<int>(n2))};
    }else if(is_real_type(n1) && is_real_type(n2)){
      static constexpr Op<double> op;
      return Lisp_ptr{new double(op(coerce<double>(n1), coerce<double>(n2)))};
    }else if(is_complex_type(n1) && is_complex_type(n2)){
      static constexpr Op<complex<double> > op;
      return Lisp_ptr{new complex<double>(op(coerce<complex<double> >(n1),
                                             coerce<complex<double> >(n2)))};
    }else{
      UNEXP_DEFAULT();
    }      
  }
};

template<typename Fun>
inline
Lisp_ptr number_divop(const char* name, Fun&& fun){
  ZsArgs args;

  for(auto i = 0; i < 2; ++i){
    if(!is_integer_type(args[i])){
      throw zs_error_arg1(name, "not integer type", {args[i]});
    }
  }
  
  return Lisp_ptr{Ptr_tag::integer,
      fun(coerce<int>(args[0]), coerce<int>(args[1]))};
}

template<typename T>
T gcd(T m, T n){
  if(m < 0) m = -m;
  if(n < 0) n = -n;

  if(m < n)
    std::swap(m, n);

  while(n > 0){
    auto mod = m % n;
    m = n;
    n = mod;
  }

  return m;
}

template<typename Fun>
inline
Lisp_ptr number_rounding(const char* name, Fun&& fun){
  ZsArgs args;

  if(!is_numeric_type(args[0])){
    throw number_type_check_failed(name, args[0]);
  }

  if(is_integer_type(args[0])){
    return args[0];
  }else if(is_real_type(args[0])){
    return Lisp_ptr{new double(fun(coerce<double>(args[0])))};
  }else if(is_complex_type(args[0])){
    throw zs_error(complex_found::msg);
  }else{
    UNEXP_DEFAULT();
  }
}

template<typename Fun>
inline
Lisp_ptr number_unary_op(const char* name, Fun&& fun){
  ZsArgs args;

  if(!is_numeric_type(args[0])){
    throw number_type_check_failed(name, args[0]);
  }

  if(is_real_type(args[0])){
    return {new double(fun(coerce<double>(args[0])))};
  }else if(is_complex_type(args[0])){
    return {new complex<double>(fun(coerce<complex<double> >(args[0])))};
  }else{
    UNEXP_DEFAULT();
  }
}

struct exp_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::exp(t1);
  }
};

struct log_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::log(t1);
  }
};

struct sin_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::sin(t1);
  }
};

struct cos_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::cos(t1);
  }
};

struct tan_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::tan(t1);
  }
};

struct asin_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::asin(t1);
  }
};

struct acos_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::acos(t1);
  }
};

struct sqrt_fun{
  template<typename T>
  inline T operator()(const T& t1) const{
    return std::sqrt(t1);
  }
};

template<typename RFun, typename CFun>
inline
Lisp_ptr number_binary_op(const char* name, RFun&& rfun, CFun&& cfun){
  ZsArgs args;
  Number* n[2];

  for(auto i = 0; i < 2; ++i){
    n[i] = args[i].get<Number*>();
    if(!n[i]){
      throw number_type_check_failed(name, args[i]);
    }
  }
  
  if(n[0]->type() == Number::Type::uninitialized
     || n[1]->type() == Number::Type::uninitialized){
    UNEXP_DEFAULT();
  }

  if(n[0]->type() <= Number::Type::real
     || n[1]->type() <= Number::Type::real){
    return {new Number(rfun(n[0]->coerce<Number::real_type>(),
                            n[1]->coerce<Number::real_type>()))};
  }

  if(n[0]->type() <= Number::Type::complex
     || n[1]->type() <= Number::Type::complex){
    return Lisp_ptr{cfun(n[0]->coerce<Number::complex_type>(),
                         n[1]->coerce<Number::complex_type>())};
  }

  UNEXP_DEFAULT();
}

template<typename Fun>
inline
Lisp_ptr number_unary_op_complex(const char* name, Fun&& fun){
  ZsArgs args;

  auto n = args[0].get<Number*>();
  if(!n){
    throw number_type_check_failed(name, args[0]);
  }

  switch(n->type()){
  case Number::Type::integer:
  case Number::Type::real:
  case Number::Type::complex:
    return {new Number(fun(n->coerce<Number::complex_type>()))};
  case Number::Type::uninitialized:
  default:
    UNEXP_DEFAULT();
  }
}

template<typename Fun>
Lisp_ptr number_i_e(const char* name, Fun&& fun){
  ZsArgs args;

  auto n = args[0].get<Number*>();
  if(!n){
    throw number_type_check_failed(name, args[0]);
  }

  return {new Number(fun(*n))};
}

} // namespace


Lisp_ptr numberp(){
  ZsArgs args;
  return Lisp_ptr{is_numeric_type(args[0])};
}

Lisp_ptr complexp(){
  ZsArgs args;
  return Lisp_ptr{is_complex_type(args[0])};
}

Lisp_ptr realp(){
  ZsArgs args;
  return Lisp_ptr{is_real_type(args[0])};
}

Lisp_ptr rationalp(){
  ZsArgs args;
  return Lisp_ptr{is_rational_type(args[0])};
}

Lisp_ptr integerp(){
  ZsArgs args;
  return Lisp_ptr{is_integer_type(args[0])};
}

Lisp_ptr exactp(){
  return number_pred([](Lisp_ptr p){
      return (p.tag() == Ptr_tag::integer);
    });
}

Lisp_ptr inexactp(){
  return number_pred([](Lisp_ptr p){
      return (p.tag() == Ptr_tag::complex || p.tag() == Ptr_tag::real);
    });
}


Lisp_ptr number_equal(){
  return number_compare("=",
                        [](Lisp_ptr p, Lisp_ptr q){
                          return eqv_internal(p, q);
                        });
}

Lisp_ptr number_less(){
  return number_compare("<",
                        number_comparator<std::less>()); 
}

Lisp_ptr number_greater(){
  return number_compare(">",
                        number_comparator<std::greater>());
}
  
Lisp_ptr number_less_eq(){
  return number_compare("<=",
                        number_comparator<std::less_equal>());
}
  
Lisp_ptr number_greater_eq(){
  return number_compare(">=",
                        number_comparator<std::greater_equal>());
}


Lisp_ptr zerop(){
  return number_pred(pos_neg_pred<std::equal_to>());
}

Lisp_ptr positivep(){
  return number_pred(pos_neg_pred<std::greater>());
}

Lisp_ptr negativep(){
  return number_pred(pos_neg_pred<std::less>());
}

Lisp_ptr oddp(){
  return number_pred(even_odd_pred<std::not_equal_to>());
}

Lisp_ptr evenp(){
  return number_pred(even_odd_pred<std::equal_to>());
}


Lisp_ptr number_max(){
  ZsArgs args;
  return number_accumulate("max", args[0], minmax_accum<std::greater>(),
                           next(begin(args)), end(args));
}

Lisp_ptr number_min(){
  ZsArgs args;
  return number_accumulate("min", args[0], minmax_accum<std::less>(),
                           next(begin(args)), end(args));
}

Lisp_ptr number_plus(){
  return number_accumulate("+", Lisp_ptr{Ptr_tag::integer, 0}, binary_accum<std::plus>());
}

Lisp_ptr number_multiple(){
  return number_accumulate("*", Lisp_ptr{Ptr_tag::integer, 1}, binary_accum<std::multiplies>());
}

Lisp_ptr number_minus(){
  ZsArgs args;

  if(!is_numeric_type(args[0])){
    throw number_type_check_failed("-", args[0]);
  }

  if(args.size() == 1){
    if(is_integer_type(args[0])){
      // TODO: add limit check
      return Lisp_ptr{Ptr_tag::integer, -coerce<int>(args[0])};
    }else if(is_real_type(args[0])){
      auto f = coerce<double>(args[0]);
      return Lisp_ptr{new double(-f)};
    }else if(is_complex_type(args[0])){
      auto c = coerce<complex<double> >(args[0]);
      return Lisp_ptr{new complex<double>(-c)};
    }else{
      UNEXP_DEFAULT();
    }
  }else{
    return number_accumulate("-", args[0], binary_accum<std::minus>(),
                             next(args.begin()), args.end());
  }
}

Lisp_ptr number_divide(){
  ZsArgs args;

  if(!is_numeric_type(args[0])){
    throw number_type_check_failed("/", args[0]);
  }

  if(args.size() == 1){
    if(is_integer_type(args[0])){
      // TODO: add type check
      return Lisp_ptr{new double(1.0 / coerce<int>(args[0]))};
    }else if(is_real_type(args[0])){
      return Lisp_ptr{new double(1.0 / coerce<double>(args[0]))};
    }else if(is_complex_type(args[0])){
      return Lisp_ptr{new complex<double>(1.0 / coerce<complex<double> >(args[0]))};
    }else{
      UNEXP_DEFAULT();
    }
  }else{
    return number_accumulate("/", args[0], binary_accum<std::divides>(),
                             next(args.begin()), args.end());
  }
}

Lisp_ptr number_abs(){
  ZsArgs args;

  if(!is_numeric_type(args[0])){
    throw number_type_check_failed("abs", args[0]);
  }

  if(is_integer_type(args[0])){
    // TODO: add INT_MIN check
    return Lisp_ptr{Ptr_tag::integer, abs(coerce<int>(args[0]))};
  }else if(is_real_type(args[0])){
    return Lisp_ptr{new double(fabs(coerce<double>(args[0])))};
  }else if(is_complex_type(args[0])){
    throw zs_error(complex_found::msg);
  }else{
    UNEXP_DEFAULT();
  }
}


Lisp_ptr number_quot(){
  return number_divop("quotient", std::divides<int>());
}

Lisp_ptr number_rem(){
  return number_divop("remainder",
                      [](int i1, int i2) -> int{
                        auto q = i1 / i2;
                        return i1 - (q * i2);
                      });
}

Lisp_ptr number_mod(){
  return number_divop("modulo", 
                      [](int i1, int i2) -> int{
                        auto m = i1 % i2;

                        if((m < 0 && i2 > 0) || (m > 0 && i2 < 0)){
                          return m + i2;
                        }else{
                          return m;
                        }
                      });
}

Lisp_ptr number_gcd(){
  return number_accumulate("gcd", Lisp_ptr(Ptr_tag::integer, 0),
                           [](Lisp_ptr n1, Lisp_ptr n2) -> Lisp_ptr {
                             if(!is_integer_type(n1) || !is_integer_type(n2)){
                               throw zs_error_arg1("gcd", "passed one is not integer.");
                             }

                             return Lisp_ptr{Ptr_tag::integer,
                                 gcd(coerce<int>(n1), coerce<int>(n2))};
                           });
}

Lisp_ptr number_lcm(){
  return number_accumulate("lcm", Lisp_ptr(Ptr_tag::integer, 1),
                           [](Lisp_ptr n1, Lisp_ptr n2) -> Lisp_ptr {
                             if(!is_integer_type(n1) || !is_integer_type(n2)){
                               throw zs_error_arg1("lcm", "passed one is not integer.");
                             }

                             auto i1 = coerce<int>(n1);
                             auto i2 = coerce<int>(n2);

                             return Lisp_ptr{Ptr_tag::integer,
                                 abs(i1 * i2 / gcd(i1, i2))};
                           });
}

Lisp_ptr number_numerator(){
  ZsArgs args;
  if(!is_numeric_type(args[0])){
    throw number_type_check_failed("numerator", args[0]);
  }

  throw zs_error("internal error: native func 'numerator' is not implemented.\n");
}

Lisp_ptr number_denominator(){
  ZsArgs args;
  if(!is_numeric_type(args[0])){
    throw number_type_check_failed("denominator", args[0]);
  }

  throw zs_error("internal error: native func 'denominator' is not implemented.\n");
}


Lisp_ptr number_floor(){
  return number_rounding("floor", [](double d){ return std::floor(d); });
}

Lisp_ptr number_ceil(){
  return number_rounding("ceiling", [](double d){ return std::ceil(d); });
}

Lisp_ptr number_trunc(){
  return number_rounding("truncate", [](double d){ return std::trunc(d); });
}

Lisp_ptr number_round(){
  return number_rounding("round", [](double d){ return std::round(d); });
}


Lisp_ptr number_rationalize(){
  ZsArgs args;
  Number* n[2];

  for(auto i = 0; i < 2; ++i){
    n[i] = args[i].get<Number*>();
    if(!n[i]){
      throw number_type_check_failed("rationalize", args[i]);
    }
  }
  
  throw zs_error("internal error: native func 'rationalize' is not implemented.\n");
}


Lisp_ptr number_exp(){
  return number_unary_op("exp", exp_fun());
}

Lisp_ptr number_log(){
  return number_unary_op("log", log_fun());
}

Lisp_ptr number_sin(){
  return number_unary_op("sin", sin_fun());
}

Lisp_ptr number_cos(){
  return number_unary_op("cos", cos_fun());
}

Lisp_ptr number_tan(){
  return number_unary_op("tan", tan_fun());
}

Lisp_ptr number_asin(){
  return number_unary_op("asin", asin_fun());
}

Lisp_ptr number_acos(){
  return number_unary_op("acos", acos_fun());
}

Lisp_ptr number_atan(){
  ZsArgs args;

  auto n1 = args[0].get<Number*>();
  if(!n1){
    throw number_type_check_failed("atan", args[0]);
  }

  switch(args.size()){
  case 1:  // std::atan()
    switch(n1->type()){
    case Number::Type::integer:
    case Number::Type::real:
      return {new Number(std::atan(n1->coerce<Number::real_type>()))};
    case Number::Type::complex: {
      return {new Number(std::atan(n1->get<Number::complex_type>()))};
    }
    case Number::Type::uninitialized:
    default:
      UNEXP_DEFAULT();
    }

  case 2: {// std::atan2()
    auto n2 = args[1].get<Number*>();
    if(!n2){
      throw number_type_check_failed("atan", args[1]);
    }

    switch(n2->type()){
    case Number::Type::integer:
    case Number::Type::real:
      return {new Number(std::atan2(n1->coerce<Number::real_type>(),
                                    n2->coerce<Number::real_type>()))};
    case Number::Type::complex:
      throw zs_error("native func: (atan <complex> <complex>) is not implemented.\n");
    case Number::Type::uninitialized:
    default:
      UNEXP_DEFAULT();
    }
  }

  default:
    throw builtin_argcount_failed("atan", 1, 2, args.size());
  }
}

Lisp_ptr number_sqrt(){
  return number_unary_op("sqrt", sqrt_fun());
}


Lisp_ptr number_expt(){
  return number_binary_op("expt",
                          [](Number::real_type n1, Number::real_type n2){
                            return std::pow(n1, n2);
                          },
                          [](const Number::complex_type& n1, const Number::complex_type& n2){
                            return Lisp_ptr{new Number(std::pow(n1, n2))};
                          });
}

Lisp_ptr number_rect(){
  return number_binary_op("make-rectangular",
                          [](Number::real_type n1, Number::real_type n2){
                            return Number::complex_type(n1, n2);
                          },
                          complex_found());
}

Lisp_ptr number_polar(){
  return number_binary_op("make-polar",
                          [](Number::real_type n1, Number::real_type n2){
                            return polar(n1, n2);
                          },
                          complex_found());
}


Lisp_ptr number_real(){
  return number_unary_op_complex("real-part",
                                 [](const Number::complex_type& z){
                                   return z.real();
                                 });
}

Lisp_ptr number_imag(){
  return number_unary_op_complex("imag-part",
                                 [](const Number::complex_type& z){
                                   return z.imag();
                                 });
}

Lisp_ptr number_mag(){
  return number_unary_op_complex("magnitude",
                                 [](const Number::complex_type& z){
                                   return std::abs(z);
                                 });
}

Lisp_ptr number_angle(){
  return number_unary_op_complex("angle",
                                 [](const Number::complex_type& z){
                                   return arg(z);
                                 });
}


Lisp_ptr number_i_to_e(){
  return number_i_e("inexact->exact", to_exact);
}

Lisp_ptr number_e_to_i(){
  return number_i_e("exact->inexact", to_inexact);
}


Lisp_ptr number_from_string(){
  ZsArgs args;

  auto str = args[0].get<String*>();
  if(!str){
    throw zs_error_arg1("string->number", "passed arg is not string", {args[0]});
  }

  int radix;

  switch(args.size()){
  case 1:
    radix = 10;
    break;
  case 2: {
    if(!is_integer_type(args[1])){
      throw zs_error_arg1("string->number", "passed radix is not integer", {args[1]});
    }
    radix = coerce<int>(args[1]);
    break;
  }
  default:
    throw builtin_argcount_failed("string->number", 1, 2, args.size());
  }

  istringstream iss(*str);

  // TODO: remove number type?
  auto n = parse_number(iss, radix);

  // copy from [reader.cc]
  switch(n.type()){
  case Number::Type::uninitialized:
    return Lisp_ptr();
  case Number::Type::integer:
    // TODO: use upper integer type!
    return Lisp_ptr(Ptr_tag::integer, n.get<long>());
  case Number::Type::real:
    return {new double(n.get<double>())};
  case Number::Type::complex:
    return {new complex<double>(n.get<complex<double> >())};
  default:
    UNEXP_DEFAULT();
  }
}

Lisp_ptr number_to_string(){
  ZsArgs args;

  if(!is_numeric_type(args[0])){
    throw zs_error_arg1("number->string", "passed arg is not number", {args[0]});
  }

  int radix;

  switch(args.size()){
  case 1:
    radix = 10;
    break;
  case 2: {
    if(!is_integer_type(args[1])){
      throw zs_error_arg1("number->string", "passed radix is not number", {args[1]});
    }
    radix = coerce<int>(args[1]);
    break;
  }
  default:
    throw builtin_argcount_failed("number->string", 1, 2, args.size());
  }

  ostringstream oss;
  print(oss, args[0], print_human_readable::f, radix);

  return {new String(oss.str())};
}
