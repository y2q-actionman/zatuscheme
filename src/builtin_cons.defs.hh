// This file is intended to be included into an array of 'BuiltinFunc'

{"pair?", {
    type_check_pair,
    {Calling::function, 1}}},

{"cons", {
    cons_cons,
    {Calling::function, 2}}},

{"car", {
    cons_car,
    {Calling::function, 1}}},
{"cdr", {
    cons_cdr,
    {Calling::function, 1}}},

{"set-car!", {
    cons_set_car,
    {Calling::function, 2}}},
{"set-cdr!", {
    cons_set_cdr,
    {Calling::function, 2}}},

{"null?", {
    cons_nullp,
    {Calling::function, 1}}},
{"list?", {
    cons_listp,
    {Calling::function, 1}}},

{"list", {
    cons_list,
    {Calling::function, 0, Variadic::t}}},
{"list*", {
    cons_list_star,
    {Calling::function, 1, Variadic::t}}},

{"length", {
    cons_length,
    {Calling::function, 1}}},

{"append", {
    cons_append,
    {Calling::function, 1, Variadic::t}}},

{"reverse", {
    cons_reverse,
    {Calling::function, 1}}},

{"list-tail", {
    cons_list_tail,
    {Calling::function, 2}}},
{"list-ref", {
    cons_list_ref,
    {Calling::function, 2}}},
