// This file is intended to be included into an array of 'BuiltinFunc'

{"pair?", {
    type_check_pair,
    {1}}},

{"cons", {
    cons_cons,
    {2}}},

{"car", {
    cons_car,
    {1}}},
{"cdr", {
    cons_cdr,
    {1}}},

{"set-car!", {
    cons_set_car,
    {2}}},
{"set-cdr!", {
    cons_set_cdr,
    {2}}},

{"null?", {
    cons_nullp,
    {1}}},
{"list?", {
    cons_listp,
    {1}}},

{"list", {
    cons_list,
    {0, Variadic::t}}},
{"list*", {
    cons_list_star,
    {1, Variadic::t}}},

{"length", {
    cons_length,
    {1}}},

{"append", {
    cons_append,
    {1, Variadic::t}}},

{"reverse", {
    cons_reverse,
    {1}}},

{"list-tail", {
    cons_list_tail,
    {2}}},
{"list-ref", {
    cons_list_ref,
    {2}}},

{"memq", {
    cons_memq,
    {2}}},
{"memv", {
    cons_memv,
    {2}}},
{"member", {
    cons_member,
    {2}}},

