#ifndef S_RULES_HH
#define S_RULES_HH

#include "lisp_ptr.hh"

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

  Env* bind(Lisp_ptr) const;

private:
  typedef std::pair<Lisp_ptr, Lisp_ptr> Rule;

  Env* env_;
  std::vector<Symbol*> literals_;
  std::vector<Rule> rules_;
};

#endif // S_RULES_HH
