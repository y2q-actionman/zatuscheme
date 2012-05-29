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
void VM_t::arg_push(Lisp_ptr p){
  args_.push_back(p);
}

inline
Lisp_ptr VM_t::arg_get(int i) const{
  return args_.at(i);
}

inline
void VM_t::arg_clear(){
  args_.clear();
}


#endif // VM_I_HH
