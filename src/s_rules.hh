#ifndef S_RULES_HH
#define S_RULES_HH

#include "lisp_ptr.hh"
#include "procedure.hh"

#include <vector>
#include <utility>

namespace Procedure{

class SyntaxRules{
public:
  typedef std::pair<Lisp_ptr, Lisp_ptr> Rule;

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

  const std::vector<Lisp_ptr>& literals() const
  { return literals_; }

  const std::vector<Rule>& rules() const
  { return rules_; }

private:
  static constexpr ProcInfo sr_procinfo
  = ProcInfo{2, 2, Passing::whole, Returning::code, MoveReturnValue::f,
             Entering::at_jump, Leaving::after_returning_op};

  Env* env_;
  std::vector<Lisp_ptr> literals_;
  std::vector<Rule> rules_;
};

std::pair<Env*, Lisp_ptr> match(const SyntaxRules&, Lisp_ptr, Env*);

} // namespace Procedure

#endif // S_RULES_HH
