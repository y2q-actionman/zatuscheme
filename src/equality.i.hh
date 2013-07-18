#ifndef EQUALITY_I_HH
#define EQUALITY_I_HH

#ifndef EQUALITY_HH
#error "Please include via parent file"
#endif

#include <functional>

inline
size_t eq_hash(Lisp_ptr p){
  return std::hash<void*>()(p.get<void*>());
}

struct EqObj {
  bool operator()(Lisp_ptr a, Lisp_ptr b) const{
    return eq_internal(a, b);
  }
};
    
struct EqHashObj{
  size_t operator()(Lisp_ptr p) const{
    return eq_hash(p);
  }
};

#endif // EQUALITY_I_HH
