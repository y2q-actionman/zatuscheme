// This file is intended to be included into an array of 'BuiltinFunc'

{"quote", {
    syntax_quote,
    {1, 1, Passing::quote}}},
{"lambda", {
    syntax_lambda,
    {1, 1, Passing::whole}}},
{"if", {
    syntax_if,
    {2, 3, Passing::quote, Returning::code, MoveReturnValue::f}}},
{"set!", {
    syntax_set,
    {2, 2, Passing::quote, Returning::code, MoveReturnValue::f}}},
{"define", {
    syntax_define,
    {1, Variadic::f, Passing::whole, Returning::pass}}},
{"begin", {
    syntax_begin,
    {1, Variadic::f, Passing::whole, Returning::code, MoveReturnValue::f}}},

{"let", {
    syntax_let,
    {1, Variadic::f, Passing::whole, Returning::pass, MoveReturnValue::f}}},
{"let*", {
    syntax_let_star,
    {1, Variadic::f, Passing::whole, Returning::pass, MoveReturnValue::f}}},
{"letrec", {
    syntax_letrec,
    {1, Variadic::f, Passing::whole, Returning::pass, MoveReturnValue::f}}},

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
    syntax_case,
    {1, Variadic::f, Passing::whole, Returning::code}}},

{"do", {
    syntax_do,
    {1, Variadic::f, Passing::whole, Returning::code}}},

{"delay", {
    syntax_delay,
    {1, Variadic::f, Passing::quote, Returning::code}}},

{"quasiquote", {
    syntax_quasiquote,
    {1, Variadic::f, Passing::whole, Returning::code}}},
{"unquote", {
    syntax_unquote,
    {1, Variadic::f, Passing::whole}}},
{"unquote-splicing", {
    syntax_unquote_splicing,
    {1, Variadic::f, Passing::whole}}},

{"else", {
    syntax_else,
    {1, Variadic::f, Passing::whole}}},
{"=>", {
    syntax_arrow,
    {1, Variadic::f, Passing::whole}}},

{"define-syntax", {
    syntax_define_syntax,
    {2, 2, Passing::quote}}},
{"let-syntax", {
    syntax_let_syntax,
    {1, Variadic::f, Passing::whole, Returning::pass, MoveReturnValue::f}}},
{"letrec-syntax", {
    syntax_letrec_syntax,
    {1, Variadic::f, Passing::whole, Returning::pass, MoveReturnValue::f}}},

{"syntax-rules", {
    syntax_syntax_rules,
    {2, 2, Passing::whole}}},
