#ifndef S_RULES_HH
#define S_RULES_HH

#include "lisp_ptr.hh"
#include "procedure.hh"

#include <vector>
#include <utility>

namespace Procedure{

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

  std::pair<Env*, Lisp_ptr> match(Lisp_ptr, Env*) const;

private:
  static constexpr ProcInfo sr_procinfo
  = ProcInfo{2, 2, Passing::whole, Returning::code, MoveReturnValue::f,
             Entering::at_jump, Leaving::after_returning_op};

  typedef std::pair<Lisp_ptr, Lisp_ptr> Rule;
  bool try_match_1(Env* env, Lisp_ptr pattern, 
                   Lisp_ptr form, Env* form_env, bool is_first) const;

  Env* env_;
  std::vector<Symbol*> literals_;
  std::vector<Rule> rules_;
};

} // namespace Procedure

#endif // S_RULES_HH
