#ifndef HASHER_I_HH
#define HASHER_I_HH

#ifndef HASHER_HH
#error "Please include via parent file"
#endif

struct eq_hash_obj{
  size_t operator()(const Lisp_ptr& p) const{
    return eq_hash(p);
  }
};

struct eq_id_hash_obj{
  size_t operator()(const Lisp_ptr& p) const{
    return eq_id_hash(p);
  }
};

#endif // HASHER_I_HH
