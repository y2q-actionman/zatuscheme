// This file is intended to be included into an array of 'BuiltinFunc'

{"vector?", {
    type_check_pred<Ptr_tag::vector>,
    {Calling::function, 1}}},
{"make-vector", {
    vector_make,
    {Calling::function, 1, 2}}},
{"vector", {
    vector_vector, 
    {Calling::function, 1, Variadic::t}}},
{"vector-length", {
    vector_length,
    {Calling::function, 1}}},
{"vector-ref", {
    vector_ref,
    {Calling::function, 2}}},
{"vector-set!", {
    vector_set,
    {Calling::function, 3}}},

{"vector->list", {
    vector_to_list,
    {Calling::function, 1}}},
{"list->vector", {
    vector_from_list,
    {Calling::function, 1}}},

{"vector-fill!", {
    vector_fill,
    {Calling::function, 2}}},
