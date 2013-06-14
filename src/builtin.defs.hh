// This file is intended to be included into an array of 'BuiltinFunc'

{"eval", {
    builtin::eval,
    {2, 2, Passing::eval, Returning::code, MoveReturnValue::f}}},

{"load", {
    builtin::load,
    {1}}},
