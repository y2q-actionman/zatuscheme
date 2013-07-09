// This file is intended to be included into an array of 'BuiltinFunc'

{"quote", {
    builtin::syntax_quote,
    {1, 1, Passing::quote}}},
{"lambda", {
    builtin::syntax_lambda,
    {1, Variadic::t, Passing::quote}}},
{"if", {
    builtin::syntax_if,
    {2, 3, Passing::quote, Returning::code, MoveReturnValue::f}}},
{"set!", {
    builtin::syntax_set,
    {2, 2, Passing::quote, Returning::code, MoveReturnValue::f}}},
{"define", {
    builtin::syntax_define,
    {1, Variadic::t, Passing::quote}}},

{"%quasiquote-list", {
    builtin::syntax_internal_quasiquote_list,
    {2, Variadic::f, Passing::quote, Returning::code}}},
{"%quasiquote-vector", {
    builtin::syntax_internal_quasiquote_vector,
    {1, Variadic::f, Passing::quote, Returning::code}}},
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
