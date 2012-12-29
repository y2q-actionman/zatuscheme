// This file is intended to be included into an array of 'BuiltinFunc'

{"vector?", {
    type_check_pred<Ptr_tag::vector>,
    {1}}},
{"make-vector", {
    vector_make,
    {1, 2}}},
{"vector", {
    vector_vector, 
    {1, Variadic::t}}},
{"vector-length", {
    vector_length,
    {1}}},
{"vector-ref", {
    vector_ref,
    {2}}},
{"vector-set!", {
    vector_set,
    {3}}},

{"vector->list", {
    vector_to_list,
    {1}}},
{"list->vector", {
    vector_from_list,
    {1}}},

{"vector-fill!", {
    vector_fill,
    {2}}},
