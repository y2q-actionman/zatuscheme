// This file is intended to be included into an array of 'NProcedure'

{"%symbol?",
    builtin::type_check_pred<Ptr_tag::symbol>,
    {1}},
{"%symbol->string",
    builtin::symbol_to_string,
    {1}},
{"%string->symbol",
    builtin::symbol_from_string,
    {1}},
