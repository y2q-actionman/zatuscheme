// This file is intended to be included into an array of 'BuiltinFunc'

{"input-port?", {
    builtin::type_check_pred<Ptr_tag::input_port>,
    {1}}},
{"output-port?", {
    builtin::type_check_pred<Ptr_tag::output_port>,
    {1}}},

{"open-input-file", {
    builtin::port_open_file_i,
    {1}}},
{"open-output-file", {
    builtin::port_open_file_o,
    {1}}},

{"close-input-port", {
    builtin::port_close_i,
    {1}}},
{"close-output-port", {
    builtin::port_close_o,
    {1}}},

{"%read", {
    builtin::port_read,
    {1}}},
{"%read-char", {
    builtin::port_read_char,
    {1}}},
{"%peek-char", {
    builtin::port_peek_char,
    {1}}},

{"eof-object?", {
    builtin::port_eof_p,
    {1}}},

{"write", {
    builtin::port_write,
    {1, 2}}},
{"display", {
    builtin::port_display,
    {1, 2}}},
{"write-char", {
    builtin::port_write_char,
    {1, 2}}},

{"%char-ready?", {
    builtin::port_char_ready,
    {1}}},
