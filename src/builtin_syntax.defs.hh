// This file is intended to be included into an array of 'BuiltinFunc'

{"quote", {
    builtin::syntax_quote,
    {1, 1, Passing::quote}}},
{"lambda", {
    builtin::syntax_lambda,
    {1, Variadic::t, Passing::quote}}},
{"if", {
    builtin::syntax_if,
    {2, 3, Passing::quote}}},
{"set!", {
    builtin::syntax_set,
    {2, 2, Passing::quote}}},
{"define", {
    builtin::syntax_define,
    {2, Variadic::t, Passing::quote}}},

{"unquote-splicing", {
    builtin::syntax_unquote_splicing,
    {1, Variadic::f, Passing::eval, Returning::stack_splice, MoveReturnValue::f}}},

{"syntax-rules", {
    builtin::syntax_syntax_rules,
    {2, 2, Passing::whole}}},

// for cond
{"%memv", {
    builtin::syntax_internal_memv,
    {2}}},

// for quasiquote
{"%list*", {
    builtin::syntax_internal_list_star,
    {1, Variadic::t}}},
{"%vector", {
    builtin::syntax_internal_vector, 
    {1, Variadic::t}}},
