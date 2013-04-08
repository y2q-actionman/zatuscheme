#include <iterator>             // next, prev
#include <algorithm>            // max, min
#include <cstdlib>              // abs
#include <cmath>
#include <sstream>
// #include <limits> // numeric_limits

#include "builtin_numeric.hh"
#include "vm.hh"
#include "builtin.hh"
#include "zs_error.hh"
#include "lisp_ptr.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "printer.hh"
#include "equality.hh"
#include "token.hh"
#include "util.hh"

using namespace std;

namespace {

zs_error number_type_check_failed(const char* func_name, Lisp_ptr p){
  return zs_error_arg1(func_name,
                       "arg is not number!",
                       {p});
}

bool is_numeric_type(Lisp_ptr p){
  auto tag = p.tag();
  return (tag == Ptr_tag::integer
          || tag == Ptr_tag::real
          || tag == Ptr_tag::complex);
}

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

//
// utilities
//

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
Complex coerce(Lisp_ptr p){
  if(p.tag() == Ptr_tag::complex){
    return *(p.get<Complex*>());
  }else{
    return Complex(coerce<double>(p), 0);
  }
}


Lisp_ptr wrap_number(int i){
  return {Ptr_tag::integer, i};
}

Lisp_ptr wrap_number(double d){
  return {new double(d)};
}

Lisp_ptr wrap_number(const Complex& z){
  return {new Complex(z)};
}

Lisp_ptr wrap_number(bool){
  UNEXP_DEFAULT();
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


//
// implementation utilities
//

template<typename IFun, typename RFun, typename CFun>
Lisp_ptr number_unary(Lisp_ptr arg1,
                      const char* name, const IFun& ifun,
                      const RFun& rfun, const CFun& cfun){
  if(!is_numeric_type(arg1)){
    throw number_type_check_failed(name, arg1);
  }

  if(is_integer_type(arg1)){
    return wrap_number(ifun(coerce<int>(arg1)));
  }else if(is_real_type(arg1)){
    return wrap_number(rfun(coerce<double>(arg1)));
  }else if(is_complex_type(arg1)){
    return wrap_number(cfun(coerce<Complex>(arg1)));
  }else{
    UNEXP_DEFAULT();
  }
}

template<typename IFun, typename RFun, typename CFun>
inline
Lisp_ptr number_unary(const char* name, const IFun& ifun,
                      const RFun& rfun, const CFun& cfun){
  ZsArgs args;
  return number_unary(args[0], name, ifun, rfun, cfun);
}


template<typename IFun, typename RFun, typename CFun>
Lisp_ptr number_binary(Lisp_ptr arg1, Lisp_ptr arg2,
                       const char* name, const IFun& ifun,
                       const RFun& rfun, const CFun& cfun){
  if(!is_numeric_type(arg1)){
    throw number_type_check_failed(name, arg1);
  }

  if(!is_numeric_type(arg2)){
    throw number_type_check_failed(name, arg2);
  }
  
  if(is_integer_type(arg1) && is_integer_type(arg2)){
    return wrap_number(ifun(coerce<int>(arg1),
                            coerce<int>(arg2)));
  }else if(is_real_type(arg1) && is_real_type(arg2)){
    return wrap_number(rfun(coerce<double>(arg1),
                            coerce<double>(arg2)));
  }else if(is_complex_type(arg1) && is_complex_type(arg2)){
    return wrap_number(cfun(coerce<Complex>(arg1),
                            coerce<Complex>(arg2)));
  }else{
    UNEXP_DEFAULT();
  }
}

template<typename IFun, typename RFun, typename CFun>
Lisp_ptr number_binary(const char* name, const IFun& ifun,
                       const RFun& rfun, const CFun& cfun){
  ZsArgs args;
  return number_binary(args[0], args[1], name, ifun, rfun, cfun);
}


template<typename IFun, typename RFun, typename CFun, typename Iter>
Lisp_ptr number_fold(const Iter& args_begin, const Iter& args_end,
                     Lisp_ptr init,
                     const char* name, const IFun& ifun,
                     const RFun& rfun, const CFun& cfun){
  auto acc = init;

  for(auto i = args_begin, e = args_end; i != e; ++i){
    acc = number_binary(acc, *i, name, ifun, rfun, cfun);
  }

  return acc;
}

template<typename IFun, typename RFun, typename CFun>
Lisp_ptr number_fold(Lisp_ptr init,
                     const char* name, const IFun& ifun,
                     const RFun& rfun, const CFun& cfun){
  ZsArgs args;
  return number_fold(begin(args), end(args), init,
                     name, ifun, rfun, cfun);
}


template<typename IFun, typename RFun, typename CFun, typename Iter>
Lisp_ptr number_all_2(const Iter& args_begin, const Iter& args_end,
                      const char* name, const IFun& ifun,
                      const RFun& rfun, const CFun& cfun){
  auto i1 = args_begin;
  auto i2 = next(i1);
  auto e = args_end;

  if(!is_numeric_type(*i1)){
    throw number_type_check_failed(name, *i1);
  }

  for(; i2 != e; i1 = i2, ++i2){
    if(!is_numeric_type(*i2)){
      throw number_type_check_failed(name, *i2);
    }

    if(is_integer_type(*i1) && is_integer_type(*i2)){
      if(!ifun(coerce<int>(*i1), coerce<int>(*i2))){
        return Lisp_ptr{false};
      }
    }else if(is_real_type(*i1) && is_real_type(*i2)){
      if(!rfun(coerce<double>(*i1), coerce<double>(*i2))){
        return Lisp_ptr{false};
      }
    }else if(is_complex_type(*i1) && is_complex_type(*i2)){
      if(!cfun(coerce<Complex>(*i1), coerce<Complex>(*i2))){
        return Lisp_ptr{false};
      }
    }else{
      UNEXP_DEFAULT();
    }
  }

  return Lisp_ptr{true};
}

template<typename IFun, typename RFun, typename CFun>
Lisp_ptr number_all_2(const char* name, const IFun& ifun,
                      const RFun& rfun, const CFun& cfun){
  ZsArgs args;
  return number_all_2(begin(args), end(args),
                      name, ifun, rfun, cfun);
}


struct inacceptable_number_type{
  template<typename T>
  bool operator()(T) const{
    throw zs_error("number error: inacceptable type\n");
  }

  template<typename T>
  bool operator()(T t, T) const{
    return operator()(t);
  }
};

struct pass_through{
  template<typename T>
  T operator()(T t) const{
    return t;
  }
};

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
  ZsArgs args;
  return Lisp_ptr{args[0].tag() == Ptr_tag::integer};
}

Lisp_ptr inexactp(){
  ZsArgs args;
  return Lisp_ptr{args[0].tag() == Ptr_tag::complex
                  || args[0].tag() == Ptr_tag::real};
}


Lisp_ptr number_equal(){
  return number_all_2("=",
                      [](int i1, int i2){ return (i1 == i2); },
                      [](double d1, double d2){ return (d1 == d2); },
                      [](Complex z1, Complex z2){ return (z1 == z2); });
}

Lisp_ptr number_less(){
  return number_all_2("<",
                      [](int i1, int i2){ return (i1 < i2); },
                      [](double d1, double d2){ return (d1 < d2); },
                      inacceptable_number_type());
}

Lisp_ptr number_greater(){
  return number_all_2(">",
                      [](int i1, int i2){ return (i1 > i2); },
                      [](double d1, double d2){ return (d1 > d2); },
                      inacceptable_number_type());
}
  
Lisp_ptr number_less_eq(){
  return number_all_2("<=",
                      [](int i1, int i2){ return (i1 <= i2); },
                      [](double d1, double d2){ return (d1 <= d2); },
                      inacceptable_number_type());
}
  
Lisp_ptr number_greater_eq(){
  return number_all_2(">",
                      [](int i1, int i2){ return (i1 >= i2); },
                      [](double d1, double d2){ return (d1 >= d2); },
                      inacceptable_number_type());
}


Lisp_ptr number_max(){
  ZsArgs args;
  return number_fold(next(args.begin()), args.end(),
                     args[0], "max",
                     [](int i1, int i2){ return max(i1, i2); },
                     [](double d1, double d2){ return max(d1, d2); },
                     inacceptable_number_type());
}

Lisp_ptr number_min(){
  ZsArgs args;
  return number_fold(next(args.begin()), args.end(),
                     args[0], "min",
                     [](int i1, int i2){ return min(i1, i2); },
                     [](double d1, double d2){ return min(d1, d2); },
                     inacceptable_number_type());
}

Lisp_ptr number_plus(){
  return number_fold(Lisp_ptr{Ptr_tag::integer, 0}, "+",
                     [](int i1, int i2){ return i1 + i2; },
                     [](double d1, double d2){ return d1 + d2; },
                     [](Complex z1, Complex z2){ return z1 + z2; });
}

Lisp_ptr number_multiple(){
  return number_fold(Lisp_ptr{Ptr_tag::integer, 1}, "*",
                     [](int i1, int i2){ return i1 * i2; },
                     [](double d1, double d2){ return d1 * d2; },
                     [](Complex z1, Complex z2){ return z1 * z2; });
}

Lisp_ptr number_minus(){
  ZsArgs args;

  if(!is_numeric_type(args[0])){
    throw number_type_check_failed("-", args[0]);
  }

  if(args.size() == 1){
    return number_unary(args[0], "-",
                        [](int i){ return -i; },
                        [](double d){ return -d; },
                        [](Complex z){ return -z; });
  }else{
    return number_fold(next(args.begin()), args.end(),
                       args[0], "-",
                       [](int i1, int i2){ return i1 - i2; },
                       [](double d1, double d2){ return d1 - d2; },
                       [](Complex z1, Complex z2){ return z1 - z2; });
  }
}

Lisp_ptr number_divide(){
  ZsArgs args;

  if(!is_numeric_type(args[0])){
    throw number_type_check_failed("/", args[0]);
  }

  if(args.size() == 1){
    return number_unary(args[0], "/",
                        [](int i){
                          return (i == 1) // integer appears only if '1 / 1'
                            ? 1
                            : 1.0 / i;
                        },
                        [](double d){ return 1.0 / d; },
                        [](Complex z){ return 1.0 / z; });
  }else{
    return number_fold(next(args.begin()), args.end(),
                       args[0], "/",
                       [](int i1, int i2){ 
                         return (i1 % i2)
                           ? static_cast<double>(i1) / i2
                           : i1 / i2;
                       },
                       [](double d1, double d2){ return d1 / d2; },
                       [](Complex z1, Complex z2){ return z1 / z2; });
  }
}

Lisp_ptr number_abs(){
  return number_unary("abs",
                      [](int i){ return std::abs(i);},
                      [](double d){ return std::abs(d);},
                      inacceptable_number_type());
}


Lisp_ptr number_quot(){
  return number_binary("quotient",
                       [](int i1, int i2){ return i1 / i2; },
                       inacceptable_number_type(),
                       inacceptable_number_type());
}

Lisp_ptr number_rem(){
  return number_binary("remainder",
                       [](int i1, int i2) -> int{
                         auto q = i1 / i2;
                         return i1 - (q * i2);
                       },
                       inacceptable_number_type(),
                       inacceptable_number_type());
}

Lisp_ptr number_mod(){
  return number_binary("modulo", 
                       [](int i1, int i2) -> int{
                         auto m = i1 % i2;

                         if((m < 0 && i2 > 0) || (m > 0 && i2 < 0)){
                           return m + i2;
                         }else{
                           return m;
                         }
                       },
                       inacceptable_number_type(),
                       inacceptable_number_type());
}

Lisp_ptr number_gcd(){
  return number_fold(Lisp_ptr{Ptr_tag::integer, 0}, "gcd",
                     [](int i1, int i2){
                       return gcd(i1, i2);
                     },
                     inacceptable_number_type(),
                     inacceptable_number_type());
}

Lisp_ptr number_lcm(){
  return number_fold(Lisp_ptr{Ptr_tag::integer, 1}, "lcm",
                     [](int i1, int i2){
                       return abs(i1 * i2 / gcd(i1, i2));
                     },
                     inacceptable_number_type(),
                     inacceptable_number_type());
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
  return number_unary("floor",
                      pass_through(),
                      [](double d){ return std::floor(d);},
                      inacceptable_number_type());
}

Lisp_ptr number_ceil(){
  return number_unary("ceiling",
                      pass_through(),
                      [](double d){ return std::ceil(d);},
                      inacceptable_number_type());
}

Lisp_ptr number_trunc(){
  return number_unary("truncate",
                      pass_through(),
                      [](double d){ return std::trunc(d);},
                      inacceptable_number_type());
}

Lisp_ptr number_round(){
  return number_unary("round",
                      pass_through(),
                      [](double d){ return std::round(d);},
                      inacceptable_number_type());
}


Lisp_ptr number_rationalize(){
  ZsArgs args;

  for(auto i = 0; i < 2; ++i){
    if(!is_numeric_type(args[i])){
      throw number_type_check_failed("rationalize", args[i]);
    }
  }
  
  throw zs_error("internal error: native func 'rationalize' is not implemented.\n");
}


Lisp_ptr number_exp(){
  return number_unary("exp",
                      [](int i){ return std::exp(i);},
                      [](double d){ return std::exp(d);},
                      [](Complex z){ return std::exp(z);});
}

Lisp_ptr number_log(){
  return number_unary("log",
                      [](int i){ return std::log(i);},
                      [](double d){ return std::log(d);},
                      [](Complex z){ return std::log(z);});
}

Lisp_ptr number_sin(){
  return number_unary("sin",
                      [](int i){ return std::sin(i);},
                      [](double d){ return std::sin(d);},
                      [](Complex z){ return std::sin(z);});
}

Lisp_ptr number_cos(){
  return number_unary("cos",
                      [](int i){ return std::cos(i);},
                      [](double d){ return std::cos(d);},
                      [](Complex z){ return std::cos(z);});
}

Lisp_ptr number_tan(){
  return number_unary("tan",
                      [](int i){ return std::tan(i);},
                      [](double d){ return std::tan(d);},
                      [](Complex z){ return std::tan(z);});
}

Lisp_ptr number_asin(){
  return number_unary("asin",
                      [](int i){ return std::asin(i);},
                      [](double d){ return std::asin(d);},
                      [](Complex z){ return std::asin(z);});
}

Lisp_ptr number_acos(){
  return number_unary("acos",
                      [](int i){ return std::acos(i);},
                      [](double d){ return std::acos(d);},
                      [](Complex z){ return std::acos(z);});
}

Lisp_ptr number_atan(){
  ZsArgs args;

  switch(args.size()){
  case 1:  // std::atan()
    return number_unary(args[0], "atan",
                        [](int i){ return std::atan(i); },
                        [](double d){ return std::atan(d); },
                        [](Complex z){ return std::atan(z); });
  case 2: // std::atan2()
    return number_binary(args[0], args[1], "atan",
                         [](int i1, int i2){
                           return std::atan2(i1, i2);
                         },
                         [](double d1, double d2){
                           return std::atan2(d1, d2);
                         },
                         inacceptable_number_type());
  default:
    throw builtin_argcount_failed("atan", 1, 2, args.size());
  }
}

Lisp_ptr number_sqrt(){
  return number_unary("sqrt",
                      [](int i){ return std::sqrt(i);},
                      [](double d){ return std::sqrt(d);},
                      [](Complex z){ return std::sqrt(z);});
}


Lisp_ptr number_expt(){
  return number_binary("expt",
                       [](int i1, int i2){
                         return std::pow(i1, i2);
                       },
                       [](double n1, double n2){
                         return std::pow(n1, n2);
                       },
                       [](Complex z1, Complex z2){
                         return std::pow(z1, z2);
                       });
}

Lisp_ptr number_rect(){
  return number_binary("make-rectangular",
                       [](int i1, int i2){
                         return Complex(i1, i2);
                       },
                       [](double n1, double n2){
                         return Complex(n1, n2);
                       },
                       inacceptable_number_type());
}

Lisp_ptr number_polar(){
  return number_binary("make-polar",
                       [](int i1, int i2){
                         return polar(static_cast<double>(i1),
                                      static_cast<double>(i2));
                       },
                       [](double n1, double n2){
                         return polar(n1, n2);
                       },
                       inacceptable_number_type());
}


Lisp_ptr number_real(){
  return number_unary("real-part",
                      inacceptable_number_type(),
                      inacceptable_number_type(),
                      [](Complex z){ return z.real();});
}

Lisp_ptr number_imag(){
  return number_unary("imag-part",
                      inacceptable_number_type(),
                      inacceptable_number_type(),
                      [](Complex z){ return z.imag();});
}

Lisp_ptr number_mag(){
  return number_unary("magnitude",
                      inacceptable_number_type(),
                      inacceptable_number_type(),
                      [](Complex z){ return std::abs(z);});
}

Lisp_ptr number_angle(){
  return number_unary("angle",
                      inacceptable_number_type(),
                      inacceptable_number_type(),
                      [](Complex z){ return arg(z);});
}


Lisp_ptr number_i_to_e(){
  // MEMO: add complex<int> type??
  return number_unary("inexact->exact",
                      pass_through(),
                      [](double d){ return static_cast<int>(d);},
                      inacceptable_number_type());
}

Lisp_ptr number_e_to_i(){
  return number_unary("exact->inexact",
                      [](int i){ return static_cast<double>(i);},
                      pass_through(),
                      pass_through());
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

  auto t = tokenize_number(iss, radix);

  if(t.type() == Token::Type::integer){
    return Lisp_ptr(Ptr_tag::integer, t.get<int>());
  }else if(t.type() == Token::Type::real){
    return {new double(t.get<double>())};
  }else if(t.type() == Token::Type::complex){
    return {new Complex(t.get<Complex>())};
  }else{
    throw zs_error_arg1("string->number", "string cannot be read as number", {args[0]});
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
