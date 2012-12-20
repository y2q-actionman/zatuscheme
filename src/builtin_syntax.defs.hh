// This file is intended to be included into an array of 'BuiltinFunc'

{"quote", {
    whole_function_quote,
    {Calling::whole_function, 1}}},
{"lambda", {
    whole_function_lambda,
    {Calling::whole_function, 1}}},
{"if", {
    whole_function_if,
    {Calling::whole_function, 1}}},
{"set!", {
    whole_function_set,
    {Calling::whole_function, 1}}},
{"define", {
    whole_function_define,
    {Calling::whole_function, 1}}},
{"begin", {
    whole_function_begin,
    {Calling::whole_function, 1}}},

{"cond", {
    whole_cond,
    {Calling::whole_function, 1}}},
{"case", {
    whole_case,
    {Calling::whole_function, 1}}},
{"and", {
    whole_and,
    {Calling::whole_function, 1}}},
{"or", {
    whole_or,
    {Calling::whole_function, 1}}},
{"let", {
    whole_function_let,
    {Calling::whole_function, 1}}},
{"let*", {
    whole_function_let_star,
    {Calling::whole_function, 1}}},
{"letrec", {
    whole_function_letrec,
    {Calling::whole_function, 1}}},
{"do", {
    whole_do,
    {Calling::whole_function, 1}}},
{"delay", {
    macro_delay,
    {Calling::macro, 1}}},

{"quasiquote", {
    whole_function_quasiquote,
    {Calling::whole_function, 1}}},
{"unquote", {
    whole_function_pass_through,
    {Calling::whole_function, 1}}},
{"unquote-splicing", {
    whole_function_error,
    {Calling::whole_function, 1}}},

{"else", {
    whole_function_error,
    {Calling::whole_function, 1}}},
{"=>", {
    whole_function_error,
    {Calling::whole_function, 1}}},
