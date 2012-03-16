#ifndef FUNCTION_HH
#define FUNCTION_HH

#include "lisp_ptr.hh"
class Env;
class Stack;

class Function {
public:
  typedef Lisp_ptr(*NativeFunc)(Env&, Stack&, int);

  enum class Type {
    interpreted,
    native
  };


  explicit Function(Lisp_ptr code, int args, bool variadic)
    : type_(Type::interpreted), required_args_(args),
      variadic_(variadic), code_(code){}
  explicit Function(NativeFunc func, int args, bool variadic)
    : type_(Type::native), required_args_(args),
      variadic_(variadic), n_func_(func){}

  
  Function(const Function&) = default;
  Function(Function&&) = default;

  Function& operator=(const Function&) = default;
  Function& operator=(Function&&) = default;

  Lisp_ptr call(Env&, Stack&, Lisp_ptr args);

private:
  Type type_; // TODO: merge these two fields into bitfields
  int required_args_;
  bool variadic_;
  union{
    Lisp_ptr code_;
    NativeFunc n_func_;
  };

  Lisp_ptr call(Env&, Stack&, int argc);
};

Function* make_function(Lisp_ptr args, Cons* code);

#endif //FUNCTION_HH
