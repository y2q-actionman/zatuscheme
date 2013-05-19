// This file is intended to be included into an array of 'BuiltinFunc'

{"char?", {
    builtin::type_check_pred<Ptr_tag::character>,
    {1}}},

{"char-ci=?", {
    builtin::char_ci_eq,
    {2}}},
{"char-ci<?", {
    builtin::char_ci_less,
    {2}}},
{"char-ci>?", {
    builtin::char_ci_greater,
    {2}}},
{"char-ci<=?", {
    builtin::char_ci_less_eq,
    {2}}},
{"char-ci>=?", {
    builtin::char_ci_greater_eq,
    {2}}},

{"char-alphabetic?", {
    builtin::char_isalpha,
    {1}}},
{"char-numeric?", {
    builtin::char_isdigit,
    {1}}},
{"char-whitespace?", {
    builtin::char_isspace,
    {1}}},
{"char-upper-case?", {
    builtin::char_isupper,
    {1}}},
{"char-lower-case?", {
    builtin::char_islower,
    {1}}},

{"char->integer", {
    builtin::char_to_int,
    {1}}},
{"integer->char", {
    builtin::char_from_int,
    {1}}},
{"char-upcase", {
    builtin::char_toupper,
    {1}}},
{"char-downcase", {
    builtin::char_tolower,
    {1}}},
