// This file is intended to be included into an array of 'BuiltinFunc'

{"procedure?", {
    type_check_procedure,
    {Calling::function, 1}}},
{"apply", {
    apply_func,
    {Calling::function, 1, Variadic::t}}},
{"force", {
    func_force,
    {Calling::function, 1}}},
{"values", {
    proc_values,
    {Calling::function, 0, Variadic::t}}},
{"call-with-values", {
    call_with_values,
    {Calling::function, 2}}},
{"call-with-current-continuation", {
    call_cc,
    {Calling::function, 1}}},
{"dynamic-wind", {
    dynamic_wind,
    {Calling::function, 3}}},
