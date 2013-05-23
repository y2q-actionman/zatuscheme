// This file is intended to be included into an array of 'BuiltinFunc'

{"error", {
    builtin::error,
    {1, 1}}},
{"with-exception-handler", {
    builtin::with_exception_handler,
    {2, 2, Passing::eval, Returning::pass, MoveReturnValue::f}}},
{"raise", {
    builtin::raise,
    {1, 1}}},
{"%unwind", {
    builtin::internal_unwind,
    {0, 0}}},
