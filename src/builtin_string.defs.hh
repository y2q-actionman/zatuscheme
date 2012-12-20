// This file is intended to be included into an array of 'BuiltinFunc'

{"string?", {
    type_check_pred<Ptr_tag::string>,
    {Calling::function, 1}}},
{"make-string", {
    string_make,
    {Calling::function, 1, 2}}},
{"string", {
    string_string,
    {Calling::function, 0, Variadic::t}}},
{"string-length", {
    string_length,
    {Calling::function, 1}}},
{"string-ref", {
    string_ref,
    {Calling::function, 2}}},
{"string-set!", {
    string_set,
    {Calling::function, 3}}},

{"string=?", {
    string_equal,
    {Calling::function, 2}}},
{"string<?", {
    string_less,
    {Calling::function, 2}}},
{"string>?", {
    string_greater,
    {Calling::function, 2}}},
{"string<=?", {
    string_less_eq,
    {Calling::function, 2}}},
{"string>=?", {
    string_greater_eq,
    {Calling::function, 2}}},
{"string-ci=?", {
    string_ci_equal,
    {Calling::function, 2}}},
{"string-ci<?", {
    string_ci_less,
    {Calling::function, 2}}},
{"string-ci>?", {
    string_ci_greater,
    {Calling::function, 2}}},
{"string-ci<=?", {
    string_ci_less_eq,
    {Calling::function, 2}}},
{"string-ci>=?", {
    string_ci_greater_eq,
    {Calling::function, 2}}},

{"substring", {
    string_substr,
    {Calling::function, 3}}},
{"string-append", {
    string_append,
    {Calling::function, 0, Variadic::t}}},

{"string->list", {
    string_to_list,
    {Calling::function, 1}}},
{"list->string", {
    string_from_list,
    {Calling::function, 1}}},

{"string-copy", {
    string_copy,
    {Calling::function, 1}}},

{"string-fill!", {
    string_fill,
    {Calling::function, 2}}},
