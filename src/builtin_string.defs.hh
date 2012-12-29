// This file is intended to be included into an array of 'BuiltinFunc'

{"string?", {
    type_check_pred<Ptr_tag::string>,
    {1}}},
{"make-string", {
    string_make,
    {1, 2}}},
{"string", {
    string_string,
    {0, Variadic::t}}},
{"string-length", {
    string_length,
    {1}}},
{"string-ref", {
    string_ref,
    {2}}},
{"string-set!", {
    string_set,
    {3}}},

{"string=?", {
    string_equal,
    {2}}},
{"string<?", {
    string_less,
    {2}}},
{"string>?", {
    string_greater,
    {2}}},
{"string<=?", {
    string_less_eq,
    {2}}},
{"string>=?", {
    string_greater_eq,
    {2}}},
{"string-ci=?", {
    string_ci_equal,
    {2}}},
{"string-ci<?", {
    string_ci_less,
    {2}}},
{"string-ci>?", {
    string_ci_greater,
    {2}}},
{"string-ci<=?", {
    string_ci_less_eq,
    {2}}},
{"string-ci>=?", {
    string_ci_greater_eq,
    {2}}},

{"substring", {
    string_substr,
    {3}}},
{"string-append", {
    string_append,
    {0, Variadic::t}}},

{"string->list", {
    string_to_list,
    {1}}},
{"list->string", {
    string_from_list,
    {1}}},

{"string-copy", {
    string_copy,
    {1}}},

{"string-fill!", {
    string_fill,
    {2}}},
