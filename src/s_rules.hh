#ifndef S_RULES_HH
#define S_RULES_HH

#include "lisp_ptr.hh"
#include "procedure.hh"

namespace zs {

class SyntaxRules{
public:
  SyntaxRules(Env*, Lisp_ptr literals, Lisp_ptr rules);
  SyntaxRules(const SyntaxRules&) = delete;
  SyntaxRules(SyntaxRules&&) = delete;

  ~SyntaxRules() = default;

  SyntaxRules& operator=(const SyntaxRules&) = delete;
  SyntaxRules& operator=(SyntaxRules&&) = delete;

  const ProcInfo* info() const
  { return &sr_procinfo; }

  Env* env() const
  { return env_; }

  const Lisp_ptr& literals() const
  { return literals_; }

  const Lisp_ptr& rules() const
  { return rules_; }

  Lisp_ptr name() const
  { return name_; }
  
  void set_name(Lisp_ptr n)
  { name_ = n; }
  
  Lisp_ptr apply(Lisp_ptr, Env*) const;

private:
  static constexpr ProcInfo sr_procinfo
  = ProcInfo{2, 2,
             proc_flag::Passing::whole,
             proc_flag::Returning::code,
             proc_flag::MoveReturnValue::f,
             // Leaving::after_returning_op
  };

  Env* const env_;
  const Lisp_ptr literals_;
  const Lisp_ptr rules_;
  Lisp_ptr name_;
};

} // namespace zs

#endif // S_RULES_HH
