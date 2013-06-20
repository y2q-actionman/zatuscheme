// This file is intended to be included into an array of 'BuiltinFunc'

{"number?", {
    builtin::numberp,
    {1}}},

{"complex?", {
    builtin::complexp,
    {1}}},
{"real?", {
    builtin::realp,
    {1}}},
{"rational?", {
    builtin::rationalp,
    {1}}},
{"integer?", {
    builtin::integerp,
    {1}}},

{"exact?", {
    builtin::exactp,
    {1}}},

{"%=", {
    builtin::internal_number_equal,
    {2}}},
{"%<", {
    builtin::internal_number_less,
    {2}}},
{"%>", {
    builtin::internal_number_greater,
    {2}}},
{"%<=", {
    builtin::internal_number_less_eq,
    {2}}},
{"%>=", {
    builtin::internal_number_greater_eq,
    {2}}},

{"%max", {
    builtin::internal_number_max,
    {2}}},
{"%min", {
    builtin::internal_number_min,
    {2}}},

{"%+", {
    builtin::internal_number_plus,
    {2}}},
{"%*", {
    builtin::internal_number_multiple,
    {2}}},
{"%-", {
    builtin::internal_number_minus,
    {2}}},
{"%/", {
    builtin::internal_number_divide,
    {2}}},

{"quotient", {
    builtin::number_quot,
    {2}}},
{"remainder", {
    builtin::number_rem,
    {2}}},
{"modulo", {
    builtin::number_mod,
    {2}}},

{"%gcd", {
    builtin::internal_number_gcd,
    {2}}},
{"%lcm", {
    builtin::internal_number_lcm,
    {2}}},

{"numerator", {
    builtin::number_numerator,
    {1}}},
{"denominator", {
    builtin::number_denominator,
    {1}}},

{"floor", {
    builtin::number_floor,
    {1}}},
{"ceiling", {
    builtin::number_ceil,
    {1}}},
{"truncate", {
    builtin::number_trunc,
    {1}}},
{"round", {
    builtin::number_round,
    {1}}},

{"rationalize", {
    builtin::number_rationalize,
    {2}}},

{"exp", {
    builtin::number_exp,
    {1}}},
{"log", {
    builtin::number_log,
    {1}}},
{"sin", {
    builtin::number_sin,
    {1}}},
{"cos", {
    builtin::number_cos,
    {1}}},
{"tan", {
    builtin::number_tan,
    {1}}},
{"asin", {
    builtin::number_asin,
    {1}}},
{"acos", {
    builtin::number_acos,
    {1}}},
{"%atan1", {
    builtin::internal_number_atan1,
    {1}}},
{"%atan2", {
    builtin::internal_number_atan2,
    {2}}},

{"sqrt", {
    builtin::number_sqrt,
    {1}}},
{"expt", {
    builtin::number_expt,
    {2}}},

{"make-rectangular", {
    builtin::number_rect,
    {2}}},
{"make-polar", {
    builtin::number_polar,
    {2}}},
{"real-part", {
    builtin::number_real,
    {1}}},
{"imag-part", {
    builtin::number_imag,
    {1}}},
{"magnitude", {
    builtin::number_mag,
    {1}}},
{"angle", {
    builtin::number_angle,
    {1}}},

{"inexact->exact", {
    builtin::number_i_to_e,
    {1}}},
{"exact->inexact", {
    builtin::number_e_to_i,
    {1}}},

{"%string->number2", {
    builtin::internal_number_from_string,
    {2}}},
{"%number->string2", {
    builtin::internal_number_to_string,
    {2}}},
