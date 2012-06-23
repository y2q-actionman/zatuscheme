#ifndef PROCEDURE_HH
#define PROCEDURE_HH

#include "lisp_ptr.hh"
#include <cstdio>

namespace Procedure {
  typedef void(*NativeFunc)();

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

  class IProcedure {
  public:
    IProcedure(Lisp_ptr code, Calling c, const ArgInfo& a, Lisp_ptr e)
      : calling_(c), argi_(a), code_(code), env_(e){}

    IProcedure(const IProcedure&) = default;
    IProcedure(IProcedure&&) = default;

    ~IProcedure() = default;
  
    IProcedure& operator=(const IProcedure&) = default;
    IProcedure& operator=(IProcedure&&) = default;

    Calling calling() const
    { return calling_; }

    const ArgInfo& arg_info() const
    { return argi_; }

    Lisp_ptr get() const
    { return code_; }

    Lisp_ptr closure() const
    { return env_; }
  
  private:
    Calling calling_;
    ArgInfo argi_;
    Lisp_ptr code_;
    Lisp_ptr env_;
  };

  class NProcedure {
  public:
    constexpr NProcedure(NativeFunc f, Calling c, const ArgInfo& a)
      : calling_(c), argi_(a), n_func_(f){}

    NProcedure(const NProcedure&) = default;
    NProcedure(NProcedure&&) = default;

    ~NProcedure() = default;
  
    NProcedure& operator=(const NProcedure&) = default;
    NProcedure& operator=(NProcedure&&) = default;


    Calling calling() const
    { return calling_; }

    const ArgInfo& arg_info() const
    { return argi_; }

    NativeFunc get() const
    { return n_func_; }

  private:
    const Calling calling_;
    const ArgInfo argi_;
    const NativeFunc n_func_;
  };
}

Procedure::ArgInfo parse_func_arg(Lisp_ptr);

const char* stringify(Procedure::Calling);

void describe(FILE*, const Procedure::ArgInfo&);

#include "procedure.i.hh"

#endif //PROCEDURE_HH
