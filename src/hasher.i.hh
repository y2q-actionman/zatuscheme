#ifndef HASHER_I_HH
#define HASHER_I_HH

#ifndef HASHER_HH
#error "Please include via parent file"
#endif

namespace std{
  template<> struct hash<Lisp_ptr>{
    size_t operator()(const Lisp_ptr& p) const{
      auto tag_hash = hash<int>()(static_cast<int>(p.tag()));

      void* val = (p.tag() == Ptr_tag::undefined) ? nullptr : p.get<void*>();
      auto val_hash = hash<void*>()(val);

      return tag_hash ^ val_hash;
    }
  };
}

#endif // HASHER_I_HH
