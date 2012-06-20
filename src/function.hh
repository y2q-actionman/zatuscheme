#ifndef FUNCTION_HH
#define FUNCTION_HH

#include "lisp_ptr.hh"
#include <cstdio>

class Function {
public:
  typedef void(*NativeFunc)();

  enum class Type {
    interpreted,
    native
  };

  enum class Calling {
    function,
    macro,
    whole_function,
    whole_macro
  };

  struct ArgInfo {
    int required_args;
    bool variadic;
    Lisp_ptr head;

    constexpr ArgInfo()
      : required_args(-1), variadic(false), head(){}
    constexpr ArgInfo(int rargs, bool v, Lisp_ptr h = Lisp_ptr())
      : required_args(rargs), variadic(v), head(h){}

    explicit operator bool() const{
      return (head) && (required_args >= 0);
    }
  };

  constexpr Function(Lisp_ptr code, Calling c, const ArgInfo& a, Lisp_ptr e)
    : type_(Type::interpreted), calling_(c), argi_(a), code_(code), env_(e){}
  constexpr Function(NativeFunc f, Calling c, const ArgInfo& a)
    : type_(Type::native), calling_(c), argi_(a), n_func_(f), env_(){}

  Function(const Function&) = default;
  Function(Function&&) = default;

  ~Function() = default;
  
  Function& operator=(const Function&) = default;
  Function& operator=(Function&&) = default;


  Type type() const
  { return type_; }

  Calling calling() const
  { return calling_; }

  const ArgInfo& arg_info() const
  { return argi_; }

  template<typename T>
  T get() const;

  Lisp_ptr closure() const
  { return env_; }
  
private:
  const Type type_;
  const Calling calling_;
  const ArgInfo argi_;
  union{
    Lisp_ptr code_;
    NativeFunc n_func_;
  };
  Lisp_ptr env_;
};

Function::ArgInfo parse_func_arg(Lisp_ptr);

const char* stringify(Function::Type);

void describe(FILE*, const Function::ArgInfo&);

#include "function.i.hh"

#endif //FUNCTION_HH
