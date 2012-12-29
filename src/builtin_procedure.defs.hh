// This file is intended to be included into an array of 'BuiltinFunc'

{"procedure?", {
    type_check_procedure,
    {1}}},
{"apply", {
    apply_func,
    {1, Variadic::t}}},
{"force", {
    func_force,
    {1}}},
{"values", {
    proc_values,
    {0, Variadic::t}}},
{"call-with-values", {
    call_with_values,
    {2}}},
{"call-with-current-continuation", {
    call_cc,
    {1}}},
{"dynamic-wind", {
    dynamic_wind,
    {3}}},
