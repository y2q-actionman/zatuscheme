#include "lisp_ptr.hh"

static_assert(sizeof(Lisp_ptr) >= sizeof(char)*2, "pointer cannot be filled by chars");
