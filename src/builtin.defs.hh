// This file is intended to be included into an array of 'BuiltinFunc'

{"eval", {
    builtin::eval,
    {2, 2, Passing::eval, Returning::code, MoveReturnValue::f}}},

{"scheme-report-environment", {
    builtin::env_r5rs,
    {1}}},
{"null-environment", {
    builtin::env_null,
    {1}}},
{"interaction-environment", {
    builtin::env_interactive,
    {0}}},

{"load", {
    builtin::load,
    {1}}},
