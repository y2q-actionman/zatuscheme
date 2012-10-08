#ifndef PROCEDURE_HH
#define PROCEDURE_HH

#include <cstdio>
#include <utility>
#include "lisp_ptr.hh"
#include "env.hh"
#include "vm.hh"

namespace Procedure {
  typedef void(*NativeFunc)();

  enum class Calling : unsigned char{
    function,
    macro,
    whole_function,
    whole_macro
  };

  enum class Variadic { f = 0, t = 1 };
  enum class Sequencial { f = 0, t = 1 };
  enum class EarlyBind { f = 0, t = 1 };

  struct ProcInfo {
    int required_args;
    Calling calling;
    bool variadic;
    bool sequencial;
    bool early_bind;

    constexpr ProcInfo(Calling c,
                       int rargs,
                       Variadic v = Variadic::f,
                       Sequencial s = Sequencial::f,
                       EarlyBind e = EarlyBind::f)
      : required_args(rargs),
        calling(c),
        variadic(static_cast<bool>(v)),
        sequencial(static_cast<bool>(s)),
        early_bind(static_cast<bool>(e)){}
  };

  std::pair<int, Variadic> parse_func_arg(Lisp_ptr);


  class IProcedure{
  public:
    IProcedure(Lisp_ptr code, const ProcInfo& pi, Lisp_ptr head, Env* e)
      : info_(pi), code_(code), arg_head_(head),  env_(e){
      if(env_) env_->add_ref();
    }

    IProcedure(const IProcedure&) = default;
    IProcedure(IProcedure&&) = default;

    ~IProcedure(){
      if(env_ && env_->release() <= 0){
        delete env_;
      }
    }
  
    IProcedure& operator=(const IProcedure&) = default;
    IProcedure& operator=(IProcedure&&) = default;

    const ProcInfo* info() const
    { return &info_; }

    Lisp_ptr arg_head() const
    { return arg_head_; }

    Lisp_ptr get() const
    { return code_; }

    Env* closure() const
    { return env_; }
  
  private:
    ProcInfo info_;
    Lisp_ptr code_;
    Lisp_ptr arg_head_;
    Env* env_;
  };

  class NProcedure{
  public:
    constexpr NProcedure(NativeFunc f, const ProcInfo& pi)
      : info_(pi), n_func_(f){}

    NProcedure(const NProcedure&) = default;
    NProcedure(NProcedure&&) = default;

    ~NProcedure() = default;
  
    NProcedure& operator=(const NProcedure&) = default;
    NProcedure& operator=(NProcedure&&) = default;

    const ProcInfo* info() const
    { return &info_; }

    NativeFunc get() const
    { return n_func_; }

  private:
    const ProcInfo info_;
    const NativeFunc n_func_;
  };

  class Continuation{
  public:
    Continuation(const VM_t&);

    Continuation(const Continuation&) = delete;
    Continuation(Continuation&&) = delete;

    ~Continuation();
  
    Continuation& operator=(const Continuation&) = delete;
    Continuation& operator=(Continuation&&) = delete;

    const ProcInfo* info() const
    { return &cont_procinfo; }

    const VM_t& get() const
    { return vm_; }

  private:
    static constexpr ProcInfo cont_procinfo = ProcInfo{Calling::function, 1, Variadic::t};
    const VM_t vm_;
  };

}

const char* stringify(Procedure::Calling);

#include "procedure.i.hh"

#endif //PROCEDURE_HH
