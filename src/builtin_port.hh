#ifndef BUILTIN_PORT_HH
#define BUILTIN_PORT_HH

#include "decl.hh"

#define CURRENT_INPUT_PORT_SYMNAME "current-input-port-value"
#define CURRENT_OUTPUT_PORT_SYMNAME "current-output-port-value"

Lisp_ptr port_open_file_i();
Lisp_ptr port_open_file_o();
Lisp_ptr port_close_i();
Lisp_ptr port_close_o();

Lisp_ptr port_read();
Lisp_ptr port_read_char();
Lisp_ptr port_peek_char();
Lisp_ptr port_eof_p();

Lisp_ptr port_write();
Lisp_ptr port_display();
Lisp_ptr port_write_char();

Lisp_ptr port_char_ready();

#endif // BUILTIN_PORT_HH
