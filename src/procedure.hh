#ifndef PROCEDURE_HH
#define PROCEDURE_HH

#include <utility>
#include <limits>
#include "lisp_ptr.hh"
#include "env.hh"
#include "vm.hh"

namespace Procedure {
  typedef Lisp_ptr(*NativeFunc)();

  enum class Calling : unsigned char{
    function,
    macro,
    whole_function
  };

  enum class Variadic : bool { f = false, t = true };
  enum class EarlyBind : bool { f = false, t = true };

  struct ProcInfo {
    int required_args;
    int max_args;
    Calling calling;
    EarlyBind early_bind;

    constexpr ProcInfo(Calling c,
                       int rargs,
                       int margs,
                       EarlyBind e = EarlyBind::f)
      : required_args(rargs),
        max_args(margs),
        calling(c),
        early_bind(e){}

    // TODO: use delegating constructor
    constexpr ProcInfo(Calling c,
                       int rargs,
                       Variadic v = Variadic::t,
                       EarlyBind e = EarlyBind::f)
      : required_args(rargs),
        max_args((v == Variadic::t) ? std::numeric_limits<decltype(max_args)>::max() : rargs),
        calling(c),
        early_bind(e){}
  };

  std::pair<int, Variadic> parse_func_arg(Lisp_ptr);

  class IProcedure{
  public:
    IProcedure(Lisp_ptr code, const ProcInfo& pi, Lisp_ptr al, Env* e)
      : info_(pi), code_(code), arg_list_(al),  env_(e){}

    IProcedure(const IProcedure&) = default;
    IProcedure(IProcedure&&) = default;

    ~IProcedure(){}
  
    IProcedure& operator=(const IProcedure&) = default;
    IProcedure& operator=(IProcedure&&) = default;

    const ProcInfo* info() const
    { return &info_; }

    Lisp_ptr arg_list() const
    { return arg_list_; }

    Lisp_ptr get() const
    { return code_; }

    Env* closure() const
    { return env_; }
  
  private:
    ProcInfo info_;
    Lisp_ptr code_;
    Lisp_ptr arg_list_;
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
    Continuation(const VM&);

    Continuation(const Continuation&) = delete;
    Continuation(Continuation&&) = delete;

    ~Continuation();
  
    Continuation& operator=(const Continuation&) = delete;
    Continuation& operator=(Continuation&&) = delete;

    const ProcInfo* info() const
    { return &cont_procinfo; }

    const VM& get() const
    { return vm_; }

  private:
    static constexpr ProcInfo cont_procinfo = ProcInfo{Calling::function, 1, Variadic::t};
    const VM vm_;
  };

  inline bool is_procedure(Lisp_ptr);
  const ProcInfo* get_procinfo(Lisp_ptr);
  Lisp_ptr get_arg_list(Lisp_ptr);

} // namespace Procedure

const char* stringify(Procedure::Calling);

#include "procedure.i.hh"

#endif //PROCEDURE_HH
