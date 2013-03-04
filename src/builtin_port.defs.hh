// This file is intended to be included into an array of 'BuiltinFunc'

{"input-port?", {
    type_check_pred<Ptr_tag::input_port>,
    {1}}},
{"output-port?", {
    type_check_pred<Ptr_tag::output_port>,
    {1}}},

{"open-input-file", {
    port_open_file_i,
    {1}}},
{"open-output-file", {
    port_open_file_o,
    {1}}},

{"close-input-port", {
    port_close_i,
    {1}}},
{"close-output-port", {
    port_close_o,
    {1}}},

{"read", {
    port_read,
    {0, 1}}},
{"read-char", {
    port_read_char,
    {0, 1}}},
{"peek-char", {
    port_peek_char,
    {0, 1}}},

{"eof-object?", {
    port_eof_p,
    {1}}},

{"write", {
    port_write,
    {1, 2}}},
{"display", {
    port_display,
    {1, 2}}},
{"write-char", {
    port_write_char,
    {1, 2}}},

{"char-ready?", {
    port_char_ready,
    {0, 1}}},
