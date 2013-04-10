// This file is intended to be included into an array of 'BuiltinFunc'

{"vector?", {
    builtin::type_check_pred<Ptr_tag::vector>,
    {1}}},
{"make-vector", {
    builtin::vector_make,
    {1, 2}}},
{"vector", {
    builtin::vector_vector, 
    {1, Variadic::t}}},
{"vector-length", {
    builtin::vector_length,
    {1}}},
{"vector-ref", {
    builtin::vector_ref,
    {2}}},
{"vector-set!", {
    builtin::vector_set,
    {3}}},

{"vector->list", {
    builtin::vector_to_list,
    {1}}},
{"list->vector", {
    builtin::vector_from_list,
    {1}}},

{"vector-fill!", {
    builtin::vector_fill,
    {2}}},
