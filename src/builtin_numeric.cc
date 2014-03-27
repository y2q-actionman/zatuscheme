#include <algorithm>            // max, min
#include <cfenv>
#include <cmath>
#include <cstdlib>              // abs
#include <functional>
#include <sstream>

#include "builtin_numeric.hh"
#include "eval.hh"
#include "lisp_ptr.hh"
#include "printer.hh"
#include "rational.hh"
#include "token.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

using namespace std;

namespace {

template<typename IFun, typename QFun, typename RFun, typename CFun>
Lisp_ptr number_unary(Lisp_ptr arg1,
                      const IFun& ifun, const QFun& qfun,
                      const RFun& rfun, const CFun& cfun){
  check_numeric_type(arg1);

  feclearexcept(FE_ALL_EXCEPT);
  if(is_numeric_convertible(arg1, Ptr_tag::integer)){
    auto ret = ifun(coerce<int>(arg1));
    if (fetestexcept(FE_INVALID))
      goto fallback_complex;
    return wrap_number(ret);
  }else if(is_numeric_convertible(arg1, Ptr_tag::rational)){
    auto ret = qfun(coerce<Rational>(arg1));
    if (fetestexcept(FE_INVALID))
      goto fallback_complex;
    return wrap_number(ret);
  }else if(is_numeric_convertible(arg1, Ptr_tag::real)){
    auto ret = rfun(coerce<double>(arg1));
    if (fetestexcept(FE_INVALID))
      goto fallback_complex;
    return wrap_number(ret);
  }else if(is_numeric_convertible(arg1, Ptr_tag::complex)){
  fallback_complex:
    return wrap_number(cfun(coerce<Complex>(arg1)));
  }else{
    UNEXP_DEFAULT();
  }
}

template<typename IFun, typename QFun, typename RFun, typename CFun>
Lisp_ptr number_binary(Lisp_ptr arg1, Lisp_ptr arg2,
                       const IFun& ifun, const QFun& qfun,
                       const RFun& rfun, const CFun& cfun){
  check_numeric_type(arg1);
  check_numeric_type(arg2);
  
  feclearexcept(FE_ALL_EXCEPT);
  if(is_numeric_convertible(arg1, Ptr_tag::integer)
     && is_numeric_convertible(arg2, Ptr_tag::integer)){
    auto ret = ifun(coerce<int>(arg1), coerce<int>(arg2));
    if (fetestexcept(FE_INVALID))
      goto fallback_complex;
    return wrap_number(ret);
  }else if(is_numeric_convertible(arg1, Ptr_tag::rational)
           && is_numeric_convertible(arg2, Ptr_tag::rational)){
    auto ret = qfun(coerce<Rational>(arg1), coerce<Rational>(arg2));
    if (fetestexcept(FE_INVALID))
      goto fallback_complex;
    return wrap_number(ret);
  }else if(is_numeric_convertible(arg1, Ptr_tag::real)
           && is_numeric_convertible(arg2, Ptr_tag::real)){
    auto ret = rfun(coerce<double>(arg1), coerce<double>(arg2));
    if (fetestexcept(FE_INVALID))
      goto fallback_complex;
    return wrap_number(ret);
  }else if(is_numeric_convertible(arg1, Ptr_tag::complex)
           && is_numeric_convertible(arg2, Ptr_tag::complex)){
  fallback_complex:
    return wrap_number(cfun(coerce<Complex>(arg1),
                            coerce<Complex>(arg2)));
  }else{
    UNEXP_DEFAULT();
  }
}

struct inacceptable_number_type{
  static bool throw_error();

  template<typename T>
  bool operator()(T) const{
    return throw_error();
  }

  template<typename T>
  bool operator()(T t, T) const{
    return operator()(t);
  }
};

bool inacceptable_number_type::throw_error(){
  throw_zs_error({}, "number error: inacceptable type");
}    


struct identity{
  template<typename T>
  T operator()(T t) const{
    return t;
  }
};

struct constant_0{
  template<typename T>
  T operator()(T) const{
    return static_cast<T>(0);
  }
};


} // namespace

namespace builtin {

Lisp_ptr numberp(ZsArgs args){
  return Lisp_ptr{is_numeric_type(args[0])};
}

Lisp_ptr internal_complexp(ZsArgs args){
  return Lisp_ptr{is_numeric_convertible(args[0], Ptr_tag::complex)};
}

Lisp_ptr internal_realp(ZsArgs args){
  return Lisp_ptr{is_numeric_convertible(args[0], Ptr_tag::real)};
}

Lisp_ptr internal_rationalp(ZsArgs args){
  return Lisp_ptr{is_numeric_convertible(args[0], Ptr_tag::rational)};
}

Lisp_ptr internal_integerp(ZsArgs args){
  return Lisp_ptr{is_numeric_convertible(args[0], Ptr_tag::integer)};
}

Lisp_ptr exactp(ZsArgs args){
  return Lisp_ptr{args[0].tag() == Ptr_tag::integer
                  || args[0].tag() == Ptr_tag::rational};
}

Lisp_ptr internal_number_equal(ZsArgs args){
  return number_binary(args[0], args[1],
                      equal_to<int>(),
                      equal_to<const Rational&>(),
                      equal_to<double>(),
                      equal_to<const Complex&>());
}

Lisp_ptr internal_number_less(ZsArgs args){
  return number_binary(args[0], args[1],
                      less<int>(),
                      less<const Rational&>(),
                      less<double>(),
                      inacceptable_number_type());
}


Lisp_ptr internal_number_max(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int i2){ return max(i1, i2); },
                       [](const Rational& q1, const Rational& q2){ return max(q1, q2); },
                       [](double d1, double d2){ return max(d1, d2); },
                       inacceptable_number_type());
}

Lisp_ptr internal_number_min(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int i2){ return min(i1, i2); },
                       [](const Rational& q1, const Rational& q2){ return min(q1, q2); },
                       [](double d1, double d2){ return min(d1, d2); },
                       inacceptable_number_type());
}

Lisp_ptr internal_number_plus(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int i2){ return Rational(i1) += Rational(i2); },
                       [](Rational&& q1, const Rational& q2){ return q1 += q2; },
                       plus<double>(),
                       plus<const Complex&>());
}

Lisp_ptr internal_number_multiple(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int i2){ return Rational(i1) *= Rational(i2); },
                       [](Rational&& q1, const Rational& q2){ return q1 *= q2; },
                       multiplies<double>(),
                       multiplies<const Complex&>());
}

Lisp_ptr internal_number_minus(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int i2){ return Rational(i1) -= Rational(i2); },
                       [](Rational&& q1, const Rational& q2){ return q1 -= q2; },
                       minus<double>(),
                       minus<const Complex&>());
}

Lisp_ptr internal_number_divide(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int i2){ return Rational(i1) /= Rational(i2); },
                       [](Rational&& q1, const Rational& q2){ return q1 /= q2; },
                       divides<double>(),
                       divides<const Complex&>());
}

Lisp_ptr number_quot(ZsArgs args){
  return number_binary(args[0], args[1],
                       divides<int>(),
                       inacceptable_number_type(),
                       inacceptable_number_type(),
                       inacceptable_number_type());
}

Lisp_ptr number_rem(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int i2) -> int{
                         auto q = i1 / i2;
                         return i1 - (q * i2);
                       },
                       inacceptable_number_type(),
                       inacceptable_number_type(),
                       inacceptable_number_type());
}

Lisp_ptr number_mod(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int i2) -> int{
                         auto m = i1 % i2;

                         if((m < 0 && i2 > 0) || (m > 0 && i2 < 0)){
                           return m + i2;
                         }else{
                           return m;
                         }
                       },
                       inacceptable_number_type(),
                       inacceptable_number_type(),
                       inacceptable_number_type());
}

Lisp_ptr internal_number_gcd(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int i2){
                         return gcd(i1, i2);
                       },
                       inacceptable_number_type(),
                       inacceptable_number_type(),
                       inacceptable_number_type());
}

Lisp_ptr number_numerator(ZsArgs args){
  return number_unary(args[0],
                      identity(),
                      [](const Rational& q){ return q.numerator(); },
                      inacceptable_number_type(),
                      inacceptable_number_type());
}

Lisp_ptr number_denominator(ZsArgs args){
  return number_unary(args[0],
                      [](int){ return 1;},
                      [](const Rational& q){ return q.denominator(); },
                      inacceptable_number_type(),
                      inacceptable_number_type());
}


Lisp_ptr number_floor(ZsArgs args){
  return number_unary(args[0],
                      identity(),
                      [](const Rational& r){ return std::floor(static_cast<double>(r));},
                      [](double d){ return std::floor(d);},
                      inacceptable_number_type());
}

Lisp_ptr number_ceil(ZsArgs args){
  return number_unary(args[0],
                      identity(),
                      [](const Rational& r){ return std::ceil(static_cast<double>(r));},
                      [](double d){ return std::ceil(d);},
                      inacceptable_number_type());
}

Lisp_ptr number_trunc(ZsArgs args){
  return number_unary(args[0],
                      identity(),
                      [](const Rational& r){ return std::trunc(static_cast<double>(r));},
                      [](double d){ return std::trunc(d);},
                      inacceptable_number_type());
}

Lisp_ptr number_round(ZsArgs args){
  return number_unary(args[0],
                      identity(),
                      [](const Rational& r){ return std::round(static_cast<double>(r));},
                      [](double d){ return std::round(d);},
                      inacceptable_number_type());
}


Lisp_ptr number_rationalize(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int){ return i1; },
                       [](const Rational& q1, const Rational&){ return q1; },
                       [](double n1, double n2){
                         return rationalize(n1, n2);
                       },
                       inacceptable_number_type());
}


Lisp_ptr number_exp(ZsArgs args){
  return number_unary(args[0],
                      [](int i){ return std::exp(i);},
                      [](const Rational& q){ return std::exp(static_cast<double>(q));},
                      [](double d){ return std::exp(d);},
                      [](const Complex& z){ return std::exp(z);});
}

Lisp_ptr number_log(ZsArgs args){
  return number_unary(args[0],
                      [](int i){ return std::log(i);},
                      [](const Rational& q){ return std::log(static_cast<double>(q));},
                      [](double d){ return std::log(d);},
                      [](const Complex& z){ return std::log(z);});
}

Lisp_ptr number_sin(ZsArgs args){
  return number_unary(args[0],
                      [](int i){ return std::sin(i);},
                      [](const Rational& q){ return std::sin(static_cast<double>(q));},
                      [](double d){ return std::sin(d);},
                      [](const Complex& z){ return std::sin(z);});
}

Lisp_ptr number_cos(ZsArgs args){
  return number_unary(args[0],
                      [](int i){ return std::cos(i);},
                      [](const Rational& q){ return std::cos(static_cast<double>(q));},
                      [](double d){ return std::cos(d);},
                      [](const Complex& z){ return std::cos(z);});
}

Lisp_ptr number_tan(ZsArgs args){
  return number_unary(args[0],
                      [](int i){ return std::tan(i);},
                      [](const Rational& q){ return std::tan(static_cast<double>(q));},
                      [](double d){ return std::tan(d);},
                      [](const Complex& z){ return std::tan(z);});
}

Lisp_ptr number_asin(ZsArgs args){
  return number_unary(args[0],
                      [](int i){ return std::asin(i);},
                      [](const Rational& q){ return std::asin(static_cast<double>(q));},
                      [](double d){ return std::asin(d);},
                      [](const Complex& z){ return std::asin(z);});
}

Lisp_ptr number_acos(ZsArgs args){
  return number_unary(args[0],
                      [](int i){ return std::acos(i);},
                      [](const Rational& q){ return std::acos(static_cast<double>(q));},
                      [](double d){ return std::acos(d);},
                      [](const Complex& z){ return std::acos(z);});
}

Lisp_ptr internal_number_atan1(ZsArgs args){
  return number_unary(args[0],
                      [](int i){ return std::atan(i); },
                      [](const Rational& q){ return std::atan(static_cast<double>(q));},
                      [](double d){ return std::atan(d); },
                      [](const Complex& z){ return std::atan(z); });
}

Lisp_ptr internal_number_atan2(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int i2){
                         return std::atan2(i1, i2);
                       },
                       [](const Rational& q1, const Rational& q2){
                         return std::atan2(static_cast<double>(q1), static_cast<double>(q2));
                       },
                       [](double d1, double d2){
                         return std::atan2(d1, d2);
                       },
                       inacceptable_number_type());
}

Lisp_ptr number_sqrt(ZsArgs args){
  return number_unary(args[0],
                      [](int i){ return std::sqrt(i);},
                      [](const Rational& q){ return std::sqrt(static_cast<double>(q));},
                      [](double d){ return std::sqrt(d);},
                      [](const Complex& z){ return std::sqrt(z);});
}

Lisp_ptr number_expt(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int i2){
                         return Rational(i1).expt(Rational(i2));
                       },
                       [](Rational&& q1, const Rational& q2){
                         return q1.expt(q2);
                       },
                       [](double n1, double n2){
			 return wrap_number(std::pow(n1, n2));
                       },
                       [](const Complex& z1, const Complex& z2){
                         return std::pow(z1, z2);
                       });
}

Lisp_ptr number_rect(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int i2){
                         return Complex(i1, i2);
                       },
                       [](const Rational& q1, const Rational& q2){
                         return Complex(static_cast<double>(q1), static_cast<double>(q2));
                       },
                       [](double n1, double n2){
                         return Complex(n1, n2);
                       },
                       inacceptable_number_type());
}

Lisp_ptr number_polar(ZsArgs args){
  return number_binary(args[0], args[1],
                       [](int i1, int i2){
                         return polar(static_cast<double>(i1), static_cast<double>(i2));
                       },
                       [](const Rational& q1, const Rational& q2){
                         return polar(static_cast<double>(q1), static_cast<double>(q2));
                       },
                       [](double n1, double n2){
                         return polar(n1, n2);
                       },
                       inacceptable_number_type());
}


Lisp_ptr number_real(ZsArgs args){
  return number_unary(args[0],
		      identity(),
		      identity(),
		      identity(),
                      [](const Complex& z){ return z.real();});
}

Lisp_ptr number_imag(ZsArgs args){
  return number_unary(args[0],
		      constant_0(),
		      constant_0(),
		      constant_0(),
                      [](const Complex& z){ return z.imag();});
}

Lisp_ptr number_mag(ZsArgs args){
  return number_unary(args[0],
		      identity(),
		      identity(),
		      identity(),
                      [](const Complex& z){ return std::abs(z);});
}

Lisp_ptr number_angle(ZsArgs args){
  return number_unary(args[0],
		      constant_0(),
		      constant_0(),
		      constant_0(),
                      [](const Complex& z){ return arg(z);});
}


Lisp_ptr number_i_to_e(ZsArgs args){
  return to_exact(args[0]);
}

Lisp_ptr number_e_to_i(ZsArgs args){
  return to_inexact(args[0]);
}


Lisp_ptr internal_number_from_string(ZsArgs args){
  check_type(Ptr_tag::string, args[0]);
  check_type(Ptr_tag::integer, args[1]);

  auto str = args[0].get<String*>();
  istringstream iss(*str);
  return parse_number(iss, coerce<int>(args[1]));
}

Lisp_ptr internal_number_to_string(ZsArgs args){
  check_numeric_type(args[0]);
  check_type(Ptr_tag::integer, args[1]);

  auto radix = args[1].get<int>();

  ostringstream oss;
  print(oss, args[0], PrintReadable::f, radix);

  return {zs_new<String>(oss.str())};
}

} // namespace builtin
