#ifndef TEST_UTIL_HH
#define TEST_UTIL_HH

#include <initializer_list>

#include "lisp_ptr.hh"


template<typename Fun>
bool test_on_print(Lisp_ptr, const char*, Fun&&);

Lisp_ptr read_from_string(const char*);
Lisp_ptr eval_text(const char*);

bool read_eval_print_test(const char* in, const char* expect);

bool eqv(Lisp_ptr, Lisp_ptr);

Lisp_ptr zs_call(std::initializer_list<Lisp_ptr> args);


template<typename Fun>
void with_expect_error(Fun);


extern int result;

int check_e(const char* input, const char* expect);


#include "test_util.i.hh"

#endif // TEST_UTIL_HH
