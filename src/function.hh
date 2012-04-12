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
  explicit constexpr Function(NativeFunc f, const ArgInfo& a)
    : type_(Type::native), argi_(a), n_func_(f){}

  Function(const Function&) = default;
  Function(Function&&) = default;

  ~Function() = default;
  
  Function& operator=(const Function&) = default;
  Function& operator=(Function&&) = default;


  const Type& type() const
  { return type_; }

  const ArgInfo& arg_info() const
  { return argi_; }

  template<typename T>
  T func() const;
  
private:
  const Type type_;
  const ArgInfo argi_;
  union{
    Lisp_ptr code_;
    NativeFunc n_func_;
  };
};

#include "function.i.hh"

#endif //FUNCTION_HH
