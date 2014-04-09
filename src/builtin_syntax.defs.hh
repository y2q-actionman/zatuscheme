// This file is intended to be included into an array of 'NProcedure'

{"%quote",
    builtin::quote,
    {1, Variadic::f, Passing::quote}},
{"%lambda",
    builtin::lambda,
    {1, Variadic::t, Passing::quote}},
{"%if",
    builtin::if_,
    {2, 3, Passing::quote}},
{"%set!",
    builtin::set,
    {2, Variadic::f, Passing::quote}},
{"%define",
    builtin::define,
    {2, Variadic::t, Passing::quote}},

{"%unquote-splicing",
    builtin::unquote_splicing,
    {1, Variadic::f, Passing::eval, Returning::stack_splice, MoveReturnValue::f}},

{"%syntax-rules",
    builtin::syntax_rules,
    {1, Variadic::t, Passing::quote}},

// for cond
{"%memv",
    builtin::memv,
    {2}},

// for quasiquote
{"%list*",
    builtin::list_star,
    {1, Variadic::t}},
