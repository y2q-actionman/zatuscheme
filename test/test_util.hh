#ifndef TEST_UTIL_HH
#define TEST_UTIL_HH

#include <cstdio>
#include "lisp_ptr.hh"
#include "vm.hh"


template<typename Fun>
bool test_on_print(Lisp_ptr, const char*, const Fun&);

Lisp_ptr read_from_string(const char*);

#include "test_util.i.hh"

#endif // TEST_UTIL_HH
