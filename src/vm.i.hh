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

#endif // VM_I_HH
