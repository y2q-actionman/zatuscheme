#ifndef TEST_UTIL_HH
#define TEST_UTIL_HH

#include <initializer_list>

#include "lisp_ptr.hh"

Lisp_ptr read_from_string(const char*);
Lisp_ptr eval_text(const char*);

bool eqv(Lisp_ptr, Lisp_ptr);

Lisp_ptr zs_call(std::initializer_list<Lisp_ptr>);


template<typename Fun>
void with_expect_error(Fun);


extern int RESULT;

// (pass) -> print -> strcmp
bool check_p(Lisp_ptr input, const char* expect);

// (pass) -> checks printable
bool check_p_success(Lisp_ptr);

// read -> print -> strcmp
bool check_r(const char* input, const char* expect);

// read -> print -> false?
bool check_r_undef(const char*);

// eval -> print -> strcmp
bool check_e(const char* input, const char* expect);

// eval -> true?
bool check_e_success(const char*);

// eval -> false?
bool check_e_undef(const char*);

// eval -> read(expect) -> eqv?
bool check_er(const char* input, const char* expect);


#include "test_util.i.hh"

#endif // TEST_UTIL_HH
