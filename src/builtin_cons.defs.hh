// This file is intended to be included into an array of 'NProcedure'

{"%pair?",
    builtin::cons_pairp,
    {1}},

{"%cons",
    builtin::cons_cons,
    {2}},

{"%car",
    builtin::cons_car,
    {1}},
{"%cdr",
    builtin::cons_cdr,
    {1}},

{"%set-car!",
    builtin::cons_set_car,
    {2}},
{"%set-cdr!",
    builtin::cons_set_cdr,
    {2}},
