// This file is intended to be included into an array of 'BuiltinFunc'

{"quote", {
    builtin::syntax_quote,
    {1, 1, Passing::quote}}},
{"lambda", {
    builtin::syntax_lambda,
    {2, Variadic::f, Passing::whole}}},
{"if", {
    builtin::syntax_if,
    {2, 3, Passing::quote, Returning::code, MoveReturnValue::f}}},
{"set!", {
    builtin::syntax_set,
    {2, 2, Passing::quote, Returning::code, MoveReturnValue::f}}},
{"define", {
    builtin::syntax_define,
    {2, Variadic::f, Passing::whole, Returning::pass}}},
{"begin", {
    builtin::syntax_begin,
    {2, Variadic::f, Passing::whole, Returning::code, MoveReturnValue::f}}},

{"delay", {
    builtin::syntax_delay,
    {1, Variadic::f, Passing::quote, Returning::code}}},

{"quasiquote", {
    builtin::syntax_quasiquote,
    {2, Variadic::f, Passing::whole, Returning::code}}},
{"unquote", {
    builtin::syntax_unquote,
    {1}}},
{"unquote-splicing", {
    builtin::syntax_unquote_splicing,
    {1, Variadic::f, Passing::eval, Returning::stack_splice, MoveReturnValue::f}}},

{"else", {
    builtin::syntax_else,
    {2, Variadic::f, Passing::whole}}},
{"=>", {
    builtin::syntax_arrow,
    {2, Variadic::f, Passing::whole}}},

{"define-syntax", {
    builtin::syntax_define_syntax,
    {2, 2, Passing::quote}}},

{"syntax-rules", {
    builtin::syntax_syntax_rules,
    {2, 2, Passing::whole}}},

// for cond
{"memv", {
    builtin::syntax_internal_memv,
    {2}}},
