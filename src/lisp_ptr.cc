#include "lisp_ptr.hh"

static_assert(sizeof(int) <= sizeof(intptr_t)
              && sizeof(bool) <= sizeof(intptr_t)
              && sizeof(char) <= sizeof(intptr_t),
              "int sizing failed");

static_assert(sizeof(intptr_t) == sizeof(void*)
              && sizeof(intptr_t) <= sizeof(const void*)
              && sizeof(intptr_t) <= sizeof(void(*)()),
              "ptr sizing failed");
