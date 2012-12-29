// This file is intended to be included into an array of 'BuiltinFunc'

{"symbol?", {
    type_check_pred<Ptr_tag::symbol>,
    {1}}},
{"symbol->string", {
    sym_to_string,
    {1}}},
{"string->symbol", {
    sym_from_string,
    {1}}},
