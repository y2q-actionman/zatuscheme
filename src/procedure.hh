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
    Lisp_ptr head;
    int required_args;
    bool variadic;
    bool sequencial;
    bool early_bind;

    constexpr ArgInfo()
      : head(), required_args(-1),
        variadic(false), sequencial(false), early_bind(false){}

    constexpr ArgInfo(int rargs, bool v, Lisp_ptr h = Lisp_ptr(),
                      bool s = false, bool e = false)
      : head(h), required_args(rargs),
        variadic(v), sequencial(s), early_bind(e){}

    explicit operator bool() const{
      return (required_args >= 0);
    }
  };

  class ProcedureBase {
  protected:
    constexpr ProcedureBase(Calling c, const ArgInfo& a)
      : calling_(c), argi_(a){}

    Calling calling() const
    { return calling_; }

    const ArgInfo& arg_info() const
    { return argi_; }

  private:
    Calling calling_;
    ArgInfo argi_;
  };

  class IProcedure : protected ProcedureBase {
  public:
    IProcedure(Lisp_ptr code, Calling c, const ArgInfo& a, Env* e)
      : ProcedureBase(c, a), code_(code), env_(e){}

    IProcedure(const IProcedure&) = default;
    IProcedure(IProcedure&&) = default;

    ~IProcedure() = default;
  
    IProcedure& operator=(const IProcedure&) = default;
    IProcedure& operator=(IProcedure&&) = default;

    using ProcedureBase::calling;
    using ProcedureBase::arg_info;

    Lisp_ptr get() const
    { return code_; }

    Env* closure() const
    { return env_; }
  
  private:
    Lisp_ptr code_;
    Env* env_;
  };

  class NProcedure : protected ProcedureBase {
  public:
    constexpr NProcedure(NativeFunc f, Calling c, const ArgInfo& a)
      : ProcedureBase(c, a), n_func_(f){}

    NProcedure(const NProcedure&) = default;
    NProcedure(NProcedure&&) = default;

    ~NProcedure() = default;
  
    NProcedure& operator=(const NProcedure&) = default;
    NProcedure& operator=(NProcedure&&) = default;

    using ProcedureBase::calling;
    using ProcedureBase::arg_info;

    NativeFunc get() const
    { return n_func_; }

  private:
    const NativeFunc n_func_;
  };
}

Procedure::ArgInfo parse_func_arg(Lisp_ptr);

const char* stringify(Procedure::Calling);

#include "procedure.i.hh"

#endif //PROCEDURE_HH
