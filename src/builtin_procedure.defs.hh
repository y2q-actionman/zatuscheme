// This file is intended to be included into an array of 'BuiltinFunc'

{"procedure?", {
    builtin::procedurep,
    {1}}},
{"apply", {
    builtin::apply,
    {1, Variadic::t, Passing::eval, Returning::pass, MoveReturnValue::f}}},
{"values", {
    builtin::values,
    {0, Variadic::t, Passing::eval, Returning::pass, MoveReturnValue::f}}},
{"call-with-values", {
    builtin::call_with_values,
    {2, 2, Passing::eval, Returning::pass, MoveReturnValue::f}}},
{"call-with-current-continuation", {
    builtin::call_cc,
    {1, 1, Passing::eval, Returning::pass, MoveReturnValue::f}}},
{"dynamic-wind", {
    builtin::dynamic_wind,
    {3, 3, Passing::eval, Returning::pass, MoveReturnValue::f}}},
