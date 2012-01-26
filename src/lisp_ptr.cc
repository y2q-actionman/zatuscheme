#include "lisp_ptr.hh"

static_assert(sizeof(Lisp_ptr) == sizeof(void*), "pointer sizing failed");
static_assert(sizeof(Lisp_ptr) >= 2, "pointer cannot be filled by chars");
static_assert(alignof(Lisp_ptr) >= 4, "pointer aligning failed");
