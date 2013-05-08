#ifndef EQUALITY_I_HH
#define EQUALITY_I_HH

#ifndef EQUALITY_HH
#error "Please include via parent file"
#endif

struct eq_obj {
  bool operator()(const Lisp_ptr& a, const Lisp_ptr& b) const{
    return eq_internal(a, b);
  }
};
    
struct eq_hash_obj{
  size_t operator()(const Lisp_ptr& p) const{
    return eq_hash(p);
  }
};

#endif // EQUALITY_I_HH
