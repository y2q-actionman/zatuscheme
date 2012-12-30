// This file is intended to be included into an array of 'BuiltinFunc'

{"quote", {
    macro_quote,
    {1, Variadic::f, Passing::quote}}},
{"lambda", {
    whole_function_lambda,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"if", {
    whole_function_if,
    {2, 3, Passing::quote, Returning::pass}}},
{"set!", {
    whole_function_set,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"define", {
    whole_function_define,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"begin", {
    whole_function_begin,
    {1, Variadic::f, Passing::whole, Returning::pass}}},

{"cond", {
    whole_cond,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"case", {
    whole_case,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"and", {
    whole_and,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"or", {
    whole_or,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"let", {
    whole_function_let,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"let*", {
    whole_function_let_star,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"letrec", {
    whole_function_letrec,
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
