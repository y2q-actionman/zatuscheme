// This file is intended to be included into an array of 'BuiltinFunc'

{"procedure?", {
    builtin::procedurep,
    {1}}},
{"apply", {
    builtin::apply,
    {1, Variadic::t}}},
{"values", {
    builtin::values,
    {0, Variadic::t, Passing::eval, Returning::pass, MoveReturnValue::f}}},
{"call-with-values", {
    builtin::call_with_values,
    {2, 2}}},
{"call-with-current-continuation", {
    builtin::call_cc,
    {1, 1}}},
{"dynamic-wind", {
    builtin::dynamic_wind,
    {3, 3}}},
