#ifndef BUILTIN_SYNTAX_HH
#define BUILTIN_SYNTAX_HH

#include "builtin.hh"

namespace builtin {

Lisp_ptr quote(ZsArgs);
Lisp_ptr lambda(ZsArgs);
Lisp_ptr if_(ZsArgs);
Lisp_ptr set(ZsArgs);
Lisp_ptr define(ZsArgs);
Lisp_ptr unquote_splicing(ZsArgs);
Lisp_ptr syntax_rules(ZsArgs);

Lisp_ptr memv(ZsArgs);
Lisp_ptr list_star(ZsArgs);

} 

#endif // BUILTIN_SYNTAX_HH
