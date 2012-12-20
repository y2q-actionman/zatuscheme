// This file is intended to be included into an array of 'BuiltinFunc'

{"char?", {
    type_check_pred<Ptr_tag::character>,
    {Calling::function, 1}}},

{"char=?", {
    char_eq,
    {Calling::function, 2}}},
{"char<?", {
    char_less,
    {Calling::function, 2}}},
{"char>?", {
    char_greater,
    {Calling::function, 2}}},
{"char<=?", {
    char_less_eq,
    {Calling::function, 2}}},
{"char>=?", {
    char_greater_eq,
    {Calling::function, 2}}},

{"char-ci=?", {
    char_ci_eq,
    {Calling::function, 2}}},
{"char-ci<?", {
    char_ci_less,
    {Calling::function, 2}}},
{"char-ci>?", {
    char_ci_greater,
    {Calling::function, 2}}},
{"char-ci<=?", {
    char_ci_less_eq,
    {Calling::function, 2}}},
{"char-ci>=?", {
    char_ci_greater_eq,
    {Calling::function, 2}}},

{"char-alphabetic?", {
    char_isalpha,
    {Calling::function, 1}}},
{"char-numeric?", {
    char_isdigit,
    {Calling::function, 1}}},
{"char-whitespace?", {
    char_isspace,
    {Calling::function, 1}}},
{"char-upper-case?", {
    char_isupper,
    {Calling::function, 1}}},
{"char-lower-case?", {
    char_islower,
    {Calling::function, 1}}},

{"char->integer", {
    char_to_int,
    {Calling::function, 1}}},
{"integer->char", {
    char_from_int,
    {Calling::function, 1}}},
{"char-upcase", {
    char_toupper,
    {Calling::function, 1}}},
{"char-downcase", {
    char_tolower,
    {Calling::function, 1}}},
