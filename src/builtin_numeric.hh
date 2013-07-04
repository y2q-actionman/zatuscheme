#ifndef BUILTIN_NUMERIC_HH
#define BUILTIN_NUMERIC_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr numberp(ZsArgs);
Lisp_ptr complexp(ZsArgs);
Lisp_ptr realp(ZsArgs);
Lisp_ptr rationalp(ZsArgs);
Lisp_ptr integerp(ZsArgs);
Lisp_ptr exactp(ZsArgs);

Lisp_ptr internal_number_equal(ZsArgs);
Lisp_ptr internal_number_less(ZsArgs);

Lisp_ptr internal_number_max(ZsArgs);
Lisp_ptr internal_number_min(ZsArgs);
Lisp_ptr internal_number_plus(ZsArgs);
Lisp_ptr internal_number_multiple(ZsArgs);
Lisp_ptr internal_number_minus(ZsArgs);
Lisp_ptr internal_number_divide(ZsArgs);

Lisp_ptr number_quot(ZsArgs);
Lisp_ptr number_rem(ZsArgs);
Lisp_ptr number_mod(ZsArgs);

Lisp_ptr internal_number_gcd(ZsArgs);

Lisp_ptr number_numerator(ZsArgs);
Lisp_ptr number_denominator(ZsArgs);

Lisp_ptr number_floor(ZsArgs);
Lisp_ptr number_ceil(ZsArgs);
Lisp_ptr number_trunc(ZsArgs);
Lisp_ptr number_round(ZsArgs);

Lisp_ptr number_rationalize(ZsArgs);

Lisp_ptr number_exp(ZsArgs);
Lisp_ptr number_log(ZsArgs);
Lisp_ptr number_sin(ZsArgs);
Lisp_ptr number_cos(ZsArgs);
Lisp_ptr number_tan(ZsArgs);
Lisp_ptr number_asin(ZsArgs);
Lisp_ptr number_acos(ZsArgs);
Lisp_ptr internal_number_atan1(ZsArgs);
Lisp_ptr internal_number_atan2(ZsArgs);

Lisp_ptr number_sqrt(ZsArgs);
Lisp_ptr number_expt(ZsArgs);
Lisp_ptr number_rect(ZsArgs);
Lisp_ptr number_polar(ZsArgs);

Lisp_ptr number_real(ZsArgs);
Lisp_ptr number_imag(ZsArgs);
Lisp_ptr number_mag(ZsArgs);
Lisp_ptr number_angle(ZsArgs);

Lisp_ptr number_i_to_e(ZsArgs);
Lisp_ptr number_e_to_i(ZsArgs);

Lisp_ptr internal_number_from_string(ZsArgs);
Lisp_ptr internal_number_to_string(ZsArgs);

}

#endif // BUILTIN_NUMERIC_HH
