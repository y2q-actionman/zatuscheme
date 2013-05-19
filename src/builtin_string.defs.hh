// This file is intended to be included into an array of 'BuiltinFunc'

{"string?", {
    builtin::type_check_pred<Ptr_tag::string>,
    {1}}},
{"make-string", {
    builtin::string_make,
    {1, 2}}},
{"string", {
    builtin::string_string,
    {0, Variadic::t}}},
{"string-length", {
    builtin::string_length,
    {1}}},
{"string-ref", {
    builtin::string_ref,
    {2}}},
{"string-set!", {
    builtin::string_set,
    {3}}},

{"string=?", {
    builtin::string_equal,
    {2}}},
{"string<?", {
    builtin::string_less,
    {2}}},
{"string>?", {
    builtin::string_greater,
    {2}}},
{"string<=?", {
    builtin::string_less_eq,
    {2}}},
{"string>=?", {
    builtin::string_greater_eq,
    {2}}},
{"string-ci=?", {
    builtin::string_ci_equal,
    {2}}},
{"string-ci<?", {
    builtin::string_ci_less,
    {2}}},
{"string-ci>?", {
    builtin::string_ci_greater,
    {2}}},
{"string-ci<=?", {
    builtin::string_ci_less_eq,
    {2}}},
{"string-ci>=?", {
    builtin::string_ci_greater_eq,
    {2}}},

{"substring", {
    builtin::string_substr,
    {3}}},
{"string-append", {
    builtin::string_append,
    {0, Variadic::t}}},

{"string->list", {
    builtin::string_to_list,
    {1}}},
{"list->string", {
    builtin::string_from_list,
    {1}}},
