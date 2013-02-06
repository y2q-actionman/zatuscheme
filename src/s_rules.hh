#ifndef S_RULES_HH
#define S_RULES_HH

#include "lisp_ptr.hh"
#include "procedure.hh"

#include <vector>
#include <utility>

class SyntaxRules{
public:
  SyntaxRules(Env*, Lisp_ptr lits, Lisp_ptr rules);
  SyntaxRules(const SyntaxRules&) = delete;
  SyntaxRules(SyntaxRules&&) = delete;

  ~SyntaxRules();

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

  Lisp_ptr apply(Lisp_ptr, Env*) const;

private:
  static constexpr ProcInfo sr_procinfo
  = ProcInfo{2, 2, ProcFlag::Passing::whole, ProcFlag::Returning::code, ProcFlag::MoveReturnValue::f,
             // Entering::at_jump, Leaving::after_returning_op};
  };

  Env* const env_;
  const Lisp_ptr literals_;
  const Lisp_ptr rules_;
};

#endif // S_RULES_HH
