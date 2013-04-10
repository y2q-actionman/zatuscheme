#ifndef BUILTIN_PORT_HH
#define BUILTIN_PORT_HH

#include "decl.hh"

#define CURRENT_INPUT_PORT_SYMNAME "current-input-port-value"
#define CURRENT_OUTPUT_PORT_SYMNAME "current-output-port-value"

namespace builtin {

Lisp_ptr port_open_file_i(ZsArgs);
Lisp_ptr port_open_file_o(ZsArgs);
Lisp_ptr port_close_i(ZsArgs);
Lisp_ptr port_close_o(ZsArgs);

Lisp_ptr port_read(ZsArgs);
Lisp_ptr port_read_char(ZsArgs);
Lisp_ptr port_peek_char(ZsArgs);
Lisp_ptr port_eof_p(ZsArgs);

Lisp_ptr port_write(ZsArgs);
Lisp_ptr port_display(ZsArgs);
Lisp_ptr port_write_char(ZsArgs);

Lisp_ptr port_char_ready(ZsArgs);

}

#endif // BUILTIN_PORT_HH
