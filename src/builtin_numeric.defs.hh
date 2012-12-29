// This file is intended to be included into an array of 'BuiltinFunc'

{"number?", {
    type_check_pred<Ptr_tag::number>,
    {1}}},

{"complex?", {
    complexp,
    {1}}},
{"real?", {
    realp,
    {1}}},
{"rational?", {
    rationalp,
    {1}}},
{"integer?", {
    integerp,
    {1}}},

{"exact?", {
    exactp,
    {1}}},
{"inexact?", {
    inexactp,
    {1}}},

{"=", {
    number_equal,
    {2, Variadic::t}}},
{"<", {
    number_less,
    {2, Variadic::t}}},
{">", {
    number_greater,
    {2, Variadic::t}}},
{"<=", {
    number_less_eq,
    {2, Variadic::t}}},
{">=", {
    number_greater_eq,
    {2, Variadic::t}}},

{"zero?", {
    zerop,
    {1}}},
{"positive?", {
    positivep,
    {1}}},
{"negative?", {
    negativep,
    {1}}},
{"odd?", {
    oddp,
    {1}}},
{"even?", {
    evenp,
    {1}}},

{"max", {
    number_max,
    {2, Variadic::t}}},
{"min", {
    number_min,
    {2, Variadic::t}}},

{"+", {
    number_plus,
    {0, Variadic::t}}},
{"*", {
    number_multiple,
    {0, Variadic::t}}},
{"-", {
    number_minus,
    {1, Variadic::t}}},
{"/", {
    number_divide,
    {1, Variadic::t}}},

{"abs", {
    number_abs,
    {1}}},

{"quotient", {
    number_quot,
    {2}}},
{"remainder", {
    number_rem,
    {2}}},
{"modulo", {
    number_mod,
    {2}}},

{"gcd", {
    number_gcd,
    {0, Variadic::t}}},
{"lcm", {
    number_lcm,
    {0, Variadic::t}}},

{"numerator", {
    number_numerator,
    {1}}},
{"denominator", {
    number_denominator,
    {1}}},

{"floor", {
    number_floor,
    {1}}},
{"ceiling", {
    number_ceil,
    {1}}},
{"truncate", {
    number_trunc,
    {1}}},
{"round", {
    number_round,
    {1}}},

{"rationalize", {
    number_rationalize,
    {1}}},

{"exp", {
    number_exp,
    {1}}},
{"log", {
    number_log,
    {1}}},
{"sin", {
    number_sin,
    {1}}},
{"cos", {
    number_cos,
    {1}}},
{"tan", {
    number_tan,
    {1}}},
{"asin", {
    number_asin,
    {1}}},
{"acos", {
    number_acos,
    {1}}},
{"atan", {
    number_atan,
    {1, 2}}},

{"sqrt", {
    number_sqrt,
    {1}}},
{"expt", {
    number_expt,
    {2}}},

{"make-rectangular", {
    number_rect,
    {2}}},
{"make-polar", {
    number_polar,
    {2}}},
{"real-part", {
    number_real,
    {1}}},
{"imag-part", {
    number_imag,
    {1}}},
{"magnitude", {
    number_mag,
    {1}}},
{"angle", {
    number_angle,
    {1}}},

{"inexact->exact", {
    number_i_to_e,
    {1}}},
{"exact->inexact", {
    number_e_to_i,
    {1}}},

{"string->number", {
    number_from_string,
    {1, 2}}},
{"number->string", {
    number_to_string,
    {1, 2}}},
