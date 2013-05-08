// This file is intended to be included into an array of 'BuiltinFunc'

{"traditional-transformer", {
    builtin::traditional_transformer,
    {1}}},
{"gensym", {
    builtin::gensym,
    {0}}},
{"sc-macro-transformer", {
    builtin::sc_macro_transformer,
    {1}}},
{"make-syntactic-closure", {
    builtin::make_syntactic_closure,
    {3}}},
{"capture-syntactic-environment", {
    builtin::capture_syntactic_environment,
    {1}}},
{"identifier?", {
    builtin::identifierp,
    {1}}},
{"identifier=?", {
    builtin::identifier_eq,
    {4}}},
{"make-synthetic-identifier", {
    builtin::make_synthetic_identifier,
    {1}}},
{"exit", {
    builtin::exit,
    {0}}},
{"transcript-on", {
    builtin::transcript_on,
    {0}}},
{"transcript-off", {
    builtin::transcript_off,
    {0}}},
{"hard-repl", {
    builtin::hard_repl,
    {0}}},
{"tmp-file", {
    builtin::tmp_file,
    {0}}},
