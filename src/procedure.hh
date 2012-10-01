#ifndef PROCEDURE_HH
#define PROCEDURE_HH

#include <cstdio>
#include "lisp_ptr.hh"
#include "env.hh"

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

  class IProcedure{
  public:
    IProcedure(Lisp_ptr code, Calling c, const ArgInfo& a, Env* e)
      : calling_(c), argi_(a), code_(code), env_(e){
      env_->add_ref();
    }

    IProcedure(const IProcedure&) = default;
    IProcedure(IProcedure&&) = default;

    ~IProcedure(){
      if(env_->release() <= 0){
        delete env_;
      }
    }
  
    IProcedure& operator=(const IProcedure&) = default;
    IProcedure& operator=(IProcedure&&) = default;

    Calling calling() const
    { return calling_; }

    const ArgInfo& arg_info() const
    { return argi_; }

    Lisp_ptr get() const
    { return code_; }

    Env* closure() const
    { return env_; }
  
  private:
    Calling calling_;
    ArgInfo argi_;
    Lisp_ptr code_;
    Env* env_;
  };

  class NProcedure{
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

#include "procedure.i.hh"

#endif //PROCEDURE_HH
