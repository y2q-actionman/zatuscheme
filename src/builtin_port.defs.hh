// This file is intended to be included into an array of 'BuiltinFunc'

{"input-port?", {
    type_check_pred<Ptr_tag::input_port>,
    {Calling::function, 1}}},
{"output-port?", {
    type_check_pred<Ptr_tag::output_port>,
    {Calling::function, 1}}},

{"open-input-file", {
    port_open_file_i,
    {Calling::function, 1}}},
{"open-output-file", {
    port_open_file_o,
    {Calling::function, 1}}},

{"close-input-port", {
    port_close_i,
    {Calling::function, 1}}},
{"close-output-port", {
    port_close_o,
    {Calling::function, 1}}},

{"read", {
    port_read,
    {Calling::function, 0, 1}}},
{"read-char", {
    port_read_char,
    {Calling::function, 0, 1}}},
{"peek-char", {
    port_peek_char,
    {Calling::function, 0, 1}}},

{"eof-object?", {
    port_eof_p,
    {Calling::function, 1}}},

{"write", {
    port_write,
    {Calling::function, 1, 2}}},
{"display", {
    port_display,
    {Calling::function, 1, 2}}},
{"write-char", {
    port_write_char,
    {Calling::function, 1, 2}}},
