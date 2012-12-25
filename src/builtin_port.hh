#ifndef BUILTIN_PORT_HH
#define BUILTIN_PORT_HH

#include "decl.hh"

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

void install_builtin_port_value();

extern const char* builtin_port_load[];
extern const size_t builtin_port_load_size;

#endif // BUILTIN_PORT_HH
