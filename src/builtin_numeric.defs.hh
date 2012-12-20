// This file is intended to be included into an array of 'BuiltinFunc'

{"number?", {
    type_check_pred<Ptr_tag::number>,
    {Calling::function, 1}}},

{"complex?", {
    complexp,
    {Calling::function, 1}}},
{"real?", {
    realp,
    {Calling::function, 1}}},
{"rational?", {
    rationalp,
    {Calling::function, 1}}},
{"integer?", {
    integerp,
    {Calling::function, 1}}},

{"exact?", {
    exactp,
    {Calling::function, 1}}},
{"inexact?", {
    inexactp,
    {Calling::function, 1}}},

{"=", {
    number_equal,
    {Calling::function, 2, Variadic::t}}},
{"<", {
    number_less,
    {Calling::function, 2, Variadic::t}}},
{">", {
    number_greater,
    {Calling::function, 2, Variadic::t}}},
{"<=", {
    number_less_eq,
    {Calling::function, 2, Variadic::t}}},
{">=", {
    number_greater_eq,
    {Calling::function, 2, Variadic::t}}},

{"zero?", {
    zerop,
    {Calling::function, 1}}},
{"positive?", {
    positivep,
    {Calling::function, 1}}},
{"negative?", {
    negativep,
    {Calling::function, 1}}},
{"odd?", {
    oddp,
    {Calling::function, 1}}},
{"even?", {
    evenp,
    {Calling::function, 1}}},

{"max", {
    number_max,
    {Calling::function, 2, Variadic::t}}},
{"min", {
    number_min,
    {Calling::function, 2, Variadic::t}}},

{"+", {
    number_plus,
    {Calling::function, 0, Variadic::t}}},
{"*", {
    number_multiple,
    {Calling::function, 0, Variadic::t}}},
{"-", {
    number_minus,
    {Calling::function, 1, Variadic::t}}},
{"/", {
    number_divide,
    {Calling::function, 1, Variadic::t}}},

{"abs", {
    number_abs,
    {Calling::function, 1}}},

{"quotient", {
    number_quot,
    {Calling::function, 2}}},
{"remainder", {
    number_rem,
    {Calling::function, 2}}},
{"modulo", {
    number_mod,
    {Calling::function, 2}}},

{"gcd", {
    number_gcd,
    {Calling::function, 0, Variadic::t}}},
{"lcm", {
    number_lcm,
    {Calling::function, 0, Variadic::t}}},

{"numerator", {
    number_numerator,
    {Calling::function, 1}}},
{"denominator", {
    number_denominator,
    {Calling::function, 1}}},

{"floor", {
    number_floor,
    {Calling::function, 1}}},
{"ceiling", {
    number_ceil,
    {Calling::function, 1}}},
{"truncate", {
    number_trunc,
    {Calling::function, 1}}},
{"round", {
    number_round,
    {Calling::function, 1}}},

{"rationalize", {
    number_rationalize,
    {Calling::function, 1}}},

{"exp", {
    number_exp,
    {Calling::function, 1}}},
{"log", {
    number_log,
    {Calling::function, 1}}},
{"sin", {
    number_sin,
    {Calling::function, 1}}},
{"cos", {
    number_cos,
    {Calling::function, 1}}},
{"tan", {
    number_tan,
    {Calling::function, 1}}},
{"asin", {
    number_asin,
    {Calling::function, 1}}},
{"acos", {
    number_acos,
    {Calling::function, 1}}},
{"atan", {
    number_atan,
    {Calling::function, 1, 2}}},

{"sqrt", {
    number_sqrt,
    {Calling::function, 1}}},
{"expt", {
    number_expt,
    {Calling::function, 2}}},

{"make-rectangular", {
    number_rect,
    {Calling::function, 2}}},
{"make-polar", {
    number_polar,
    {Calling::function, 2}}},
{"real-part", {
    number_real,
    {Calling::function, 1}}},
{"imag-part", {
    number_imag,
    {Calling::function, 1}}},
{"magnitude", {
    number_mag,
    {Calling::function, 1}}},
{"angle", {
    number_angle,
    {Calling::function, 1}}},

{"inexact->exact", {
    number_i_to_e,
    {Calling::function, 1}}},
{"exact->inexact", {
    number_e_to_i,
    {Calling::function, 1}}},

{"string->number", {
    number_from_string,
    {Calling::function, 1, 2}}},
{"number->string", {
    number_to_string,
    {Calling::function, 1, 2}}},
