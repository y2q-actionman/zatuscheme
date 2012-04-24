#ifndef TEST_UTIL_HH
#define TEST_UTIL_HH

#include <cstdio>

#include "lisp_ptr.hh"
#include "symtable.hh"


struct FILE_deleter;
struct malloc_deleter;

template<typename Fun>
bool test_on_print(Lisp_ptr, const char*, const Fun&);

Lisp_ptr read_from_string(SymTable&, const char*);

#include "test_util.i.hh"

#endif // TEST_UTIL_HH
