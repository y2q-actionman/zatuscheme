#ifndef FUNCTION_HH
#define FUNCTION_HH

#include "lisp_ptr.hh"
class Env;
class Stack;

class Function {
public:
  explicit Function(Lisp_ptr code)
    : code_(code){}
  
  Function(const Function&) = default;
  Function(Function&&) = default;

  Function& operator=(const Function&) = default;
  Function& operator=(Function&&) = default;

  Lisp_ptr call(Env&, Stack&);
  Lisp_ptr call(Env&, Stack&, Lisp_ptr args);

private:
  Lisp_ptr code_;
};

Function* make_function(Lisp_ptr args, Cons* code);

#endif //FUNCTION_HH
