#ifndef BUILTIN_VECTOR_HH
#define BUILTIN_VECTOR_HH

#include "decl.hh"

namespace builtin {

Lisp_ptr vector_make(ZsArgs);
Lisp_ptr vector_vector(ZsArgs);
Lisp_ptr vector_length(ZsArgs);
Lisp_ptr vector_ref(ZsArgs);
Lisp_ptr vector_set(ZsArgs);
Lisp_ptr vector_to_list(ZsArgs);
Lisp_ptr vector_from_list(ZsArgs);
Lisp_ptr vector_fill(ZsArgs);

}

#endif // BUILTIN_VECTOR_HH
