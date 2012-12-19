// This file is intended to be included into an array of 'BuiltinFunc'

{"boolean?", {
    type_check_pred<Ptr_tag::boolean>, 
    {Calling::function, 1}}},
{"not", {
    not_func,
    {Calling::function, 1}}},
