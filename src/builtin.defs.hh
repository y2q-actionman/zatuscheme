// This file is intended to be included into an array of 'BuiltinFunc'

{"eval", {
    eval_func,
    {2, 2, Passing::eval, Returning::code, MoveReturnValue::f}}},

{"scheme-report-environment", {
    env_r5rs,
    {1}}},
{"null-environment", {
    env_null,
    {1}}},
{"interaction-environment", {
    env_interactive,
    {0}}},

{"load", {
    load_func,
    {1}}},
