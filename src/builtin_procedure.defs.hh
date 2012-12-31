// This file is intended to be included into an array of 'BuiltinFunc'

{"procedure?", {
    type_check_procedure,
    {1}}},
{"apply", {
    apply_func,
    {1, Variadic::t, Passing::eval, Returning::pass, MoveReturnValue::f}}},
{"force", {
    func_force,
    {1, 1, Passing::eval, Returning::pass, MoveReturnValue::f}}},
{"values", {
    proc_values,
    {0, Variadic::t, Passing::eval, Returning::pass, MoveReturnValue::f}}},
{"call-with-values", {
    call_with_values,
    {2, 2, Passing::eval, Returning::pass, MoveReturnValue::f}}},
{"call-with-current-continuation", {
    call_cc,
    {1, 1, Passing::eval, Returning::pass, MoveReturnValue::f}}},
{"dynamic-wind", {
    dynamic_wind,
    {3}}},
