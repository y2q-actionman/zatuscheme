#ifndef VM_I_HH
#define VM_I_HH

#ifndef VM_HH
#error "Please include via parent file"
#endif

inline
Lisp_ptr VM_t::find(Symbol* s){
  return traverse(s, Lisp_ptr{});
}

inline
void VM_t::set(Symbol* s, Lisp_ptr p){
  traverse(s, p);
}

inline
void VM_t::local_set(Symbol* s, Lisp_ptr p){
  frame->local_set(s, p);
}

inline
Lisp_ptr VM_t::traverse(Symbol* s, Lisp_ptr p){
  return frame->traverse(s, p);
}

#endif // VM_I_HH
