#ifndef FUNCTION_HH
#define FUNCTION_HH

#include "lisp_ptr.hh"
class Env;
class Stack;

class Function {
public:
  explicit Function(Lisp_ptr p)
    : code_(p){}
  
  Function(const Function&) = default;
  Function(Function&&) = default;

  Function& operator=(const Function&) = default;
  Function& operator=(Function&&) = default;

  Lisp_ptr call(Env&, Stack&);

private:
  Lisp_ptr code_;
};

#endif //FUNCTION_HH
