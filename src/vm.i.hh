#ifndef VM_I_HH
#define VM_I_HH

#ifndef VM_HH
#error "Please include via parent file"
#endif

inline
void VM_t::enter_frame(Lisp_ptr p){
  frame_history_.push(frame_);
  frame_ = p;
}  

inline
void VM_t::leave_frame(){
  frame_ = frame_history_.top();
  frame_history_.pop();
}  

inline
Lisp_ptr VM_t::find(Symbol* s){
  return traverse(s, Lisp_ptr{});
}

inline
void VM_t::set(Symbol* s, Lisp_ptr p){
  auto old = traverse(s, p);
  if(!old) local_set(s, p);
}

#endif // VM_I_HH
