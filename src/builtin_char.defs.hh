// This file is intended to be included into an array of 'BuiltinFunc'

{"char?", {
    type_check_pred<Ptr_tag::character>,
    {1}}},

{"char=?", {
    char_eq,
    {2}}},
{"char<?", {
    char_less,
    {2}}},
{"char>?", {
    char_greater,
    {2}}},
{"char<=?", {
    char_less_eq,
    {2}}},
{"char>=?", {
    char_greater_eq,
    {2}}},

{"char-ci=?", {
    char_ci_eq,
    {2}}},
{"char-ci<?", {
    char_ci_less,
    {2}}},
{"char-ci>?", {
    char_ci_greater,
    {2}}},
{"char-ci<=?", {
    char_ci_less_eq,
    {2}}},
{"char-ci>=?", {
    char_ci_greater_eq,
    {2}}},

{"char-alphabetic?", {
    char_isalpha,
    {1}}},
{"char-numeric?", {
    char_isdigit,
    {1}}},
{"char-whitespace?", {
    char_isspace,
    {1}}},
{"char-upper-case?", {
    char_isupper,
    {1}}},
{"char-lower-case?", {
    char_islower,
    {1}}},

{"char->integer", {
    char_to_int,
    {1}}},
{"integer->char", {
    char_from_int,
    {1}}},
{"char-upcase", {
    char_toupper,
    {1}}},
{"char-downcase", {
    char_tolower,
    {1}}},
