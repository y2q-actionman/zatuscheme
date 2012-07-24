#ifndef TEST_UTIL_HH
#define TEST_UTIL_HH

#include <cstdio>
#include "lisp_ptr.hh"
#include "vm.hh"


template<typename Fun>
bool test_on_print(Lisp_ptr, const char*, Fun&&);

Lisp_ptr read_from_string(const char*);
Lisp_ptr eval_text(const char*);

template<typename Fun>
bool read_eval_print_test(const char* in, const char* expect, Fun&&);


bool eqv(Lisp_ptr, Lisp_ptr);

#include "test_util.i.hh"

#endif // TEST_UTIL_HH
