#ifndef BUILTIN_EQUAL_HH
#define BUILTIN_EQUAL_HH

#include "decl.hh"

bool eq_internal(Lisp_ptr, Lisp_ptr);
// bool eq_id_internal(Lisp_ptr, Lisp_ptr);
bool eqv_internal(Lisp_ptr, Lisp_ptr);
bool equal_internal(Lisp_ptr, Lisp_ptr);

template<typename T>
struct eq_obj {
  bool operator()(const T& a, const T& b) const{
    return eq_internal(a, b);
  }
};
    

Lisp_ptr eq();
Lisp_ptr eqv();
Lisp_ptr equal();

#endif // BUILTIN_EQUAL_HH
