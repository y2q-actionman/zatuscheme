#ifndef VM_I_HH
#define VM_I_HH

#ifndef VM_HH
#error "Please include via parent file"
#endif

inline
Lisp_ptr VM::find(Symbol* s){
  return frame->traverse(s, Lisp_ptr{});
}

inline
void VM::set(Symbol* s, Lisp_ptr p){
  frame->traverse(s, p);
}

inline
void VM::local_set(Symbol* s, Lisp_ptr p){
  frame->local_set(s, p);
}

#endif // VM_I_HH
