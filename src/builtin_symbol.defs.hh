// This file is intended to be included into an array of 'BuiltinFunc'

{"symbol?", {
    type_check_pred<Ptr_tag::symbol>,
    {Calling::function, 1}}},
{"symbol->string", {
    sym_to_string,
    {Calling::function, 1}}},
{"string->symbol", {
    sym_from_string,
    {Calling::function, 1}}},
