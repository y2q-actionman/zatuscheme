#ifndef BUILTIN_NUMERIC_HH
#define BUILTIN_NUMERIC_HH

#include "lisp_ptr.hh"

Lisp_ptr complexp();
Lisp_ptr realp();
Lisp_ptr rationalp();
Lisp_ptr integerp();
Lisp_ptr exactp();
Lisp_ptr inexactp();

Lisp_ptr number_equal();
Lisp_ptr number_less();
Lisp_ptr number_greater();
Lisp_ptr number_less_eq();
Lisp_ptr number_greater_eq();
Lisp_ptr zerop();
Lisp_ptr positivep();
Lisp_ptr negativep();
Lisp_ptr oddp();
Lisp_ptr evenp();

Lisp_ptr number_max();
Lisp_ptr number_min();
Lisp_ptr number_plus();
Lisp_ptr number_multiple();
Lisp_ptr number_minus();
Lisp_ptr number_divide();
Lisp_ptr number_abs();

Lisp_ptr number_quot();
Lisp_ptr number_rem();
Lisp_ptr number_mod();

Lisp_ptr number_gcd();
Lisp_ptr number_lcm();

Lisp_ptr number_numerator();
Lisp_ptr number_denominator();

Lisp_ptr number_floor();
Lisp_ptr number_ceil();
Lisp_ptr number_trunc();
Lisp_ptr number_round();

Lisp_ptr number_rationalize();

Lisp_ptr number_exp();
Lisp_ptr number_log();
Lisp_ptr number_sin();
Lisp_ptr number_cos();
Lisp_ptr number_tan();
Lisp_ptr number_asin();
Lisp_ptr number_acos();
Lisp_ptr number_atan();
Lisp_ptr number_sqrt();

Lisp_ptr number_expt();
Lisp_ptr number_rect();
Lisp_ptr number_polar();

Lisp_ptr number_real();
Lisp_ptr number_imag();
Lisp_ptr number_mag();
Lisp_ptr number_angle();

Lisp_ptr number_i_to_e();
Lisp_ptr number_e_to_i();

Lisp_ptr number_from_string();
Lisp_ptr number_to_string();

#endif // BUILTIN_NUMERIC_HH
