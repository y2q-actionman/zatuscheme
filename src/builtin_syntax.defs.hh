// This file is intended to be included into an array of 'BuiltinFunc'

{"quote", {
    syntax_quote,
    {1, Variadic::f, Passing::quote}}},
{"lambda", {
    syntax_lambda,
    {1, Variadic::f, Passing::whole}}},
{"if", {
    syntax_if,
    {2, 3, Passing::quote, Returning::code, MoveReturnValue::f}}},
{"set!", {
    syntax_set,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"define", {
    syntax_define,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"begin", {
    syntax_begin,
    {1, Variadic::f, Passing::whole, Returning::pass}}},

{"let", {
    syntax_let,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"let*", {
    syntax_let_star,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"letrec", {
    syntax_letrec,
    {1, Variadic::f, Passing::whole, Returning::pass}}},

{"cond", {
    syntax_cond,
    {1, Variadic::f, Passing::whole, Returning::code}}},
{"and", {
    syntax_and,
    {1, Variadic::f, Passing::whole, Returning::code}}},
{"or", {
    syntax_or,
    {1, Variadic::f, Passing::whole, Returning::code}}},

{"case", {
    whole_case,
    {1, Variadic::f, Passing::whole, Returning::pass}}},

{"do", {
    whole_do,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"delay", {
    macro_delay,
    {1, Variadic::f, Passing::quote, Returning::code}}},

{"quasiquote", {
    whole_function_quasiquote,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"unquote", {
    whole_function_pass_through,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"unquote-splicing", {
    whole_function_error,
    {1, Variadic::f, Passing::whole, Returning::pass}}},

{"else", {
    whole_function_error,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"=>", {
    whole_function_error,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
