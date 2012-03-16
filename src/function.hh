#ifndef FUNCTION_HH
#define FUNCTION_HH

#include "lisp_ptr.hh"
class Env;
class Stack;

class Function {
public:
  typedef Lisp_ptr(*NativeFunc)(Env&, Stack&);

  enum class Type {
    interpreted,
    native
  };


  explicit Function(Lisp_ptr code, int args)
    : type_(Type::interpreted), args_(args), code_(code){}
  explicit Function(NativeFunc func, int args)
    : type_(Type::native), args_(args), n_func_(func){}

  
  Function(const Function&) = default;
  Function(Function&&) = default;

  Function& operator=(const Function&) = default;
  Function& operator=(Function&&) = default;

  Lisp_ptr call(Env&, Stack&, Lisp_ptr args);

private:
  Type type_; // TODO: merge these two fields into bitfields
  int args_;
  union{
    Lisp_ptr code_;
    NativeFunc n_func_;
  };

  Lisp_ptr call(Env&, Stack&, int argc);
};

Function* make_function(Lisp_ptr args, Cons* code);

#endif //FUNCTION_HH
