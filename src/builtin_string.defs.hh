// This file is intended to be included into an array of 'BuiltinFunc'

{"string?", {
    builtin::type_check_pred<Ptr_tag::string>,
    {1}}},
{"make-string", {
    builtin::string_make,
    {1, 2}}},
{"string-length", {
    builtin::string_length,
    {1}}},
{"string-ref", {
    builtin::string_ref,
    {2}}},
{"string-set!", {
    builtin::string_set,
    {3}}},

{"%string-strcmp", {
    builtin::internal_string_strcmp,
    {2}}},
{"%string-strcasecmp", {
    builtin::internal_string_strcasecmp,
    {2}}},
