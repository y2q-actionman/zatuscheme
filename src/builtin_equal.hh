#ifndef BUILTIN_EQUAL_HH
#define BUILTIN_EQUAL_HH

#include "lisp_ptr.hh"

bool eq_internal(Lisp_ptr, Lisp_ptr);
bool eq_id_internal(Lisp_ptr, Lisp_ptr);
bool eqv_internal(Lisp_ptr, Lisp_ptr);
bool equal_internal(Lisp_ptr, Lisp_ptr);

struct eq_obj {
  bool operator()(const Lisp_ptr& a, const Lisp_ptr& b) const{
    return eq_internal(a, b);
  }
};
    
struct eq_id_obj {
  bool operator()(const Lisp_ptr& a, const Lisp_ptr& b) const{
    return eq_id_internal(a, b);
  }
};
    

Lisp_ptr eq();
Lisp_ptr eqv();
Lisp_ptr equal();

#endif // BUILTIN_EQUAL_HH
