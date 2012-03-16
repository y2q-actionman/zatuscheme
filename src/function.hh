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

  struct ArgInfo {
    bool valid;
    bool variadic;
    int required_args;
  };


  explicit Function(Lisp_ptr code, const ArgInfo& a)
    : type_(Type::interpreted), argi_(a), code_(code){}
  explicit Function(NativeFunc func, const ArgInfo& a)
    : type_(Type::native), argi_(a), n_func_(func){}

  ~Function() = default;
  
  Function(const Function&) = default;
  Function(Function&&) = default;

  Function& operator=(const Function&) = default;
  Function& operator=(Function&&) = default;

  Lisp_ptr call(Env&, Stack&, Lisp_ptr args);

private:
  const Type type_;
  const ArgInfo argi_;
  union{
    Lisp_ptr code_;
    NativeFunc n_func_;
  };

  Lisp_ptr call(Env&, Stack&, int argc);
};

Function::ArgInfo parse_func_arg(Lisp_ptr args);

#endif //FUNCTION_HH
