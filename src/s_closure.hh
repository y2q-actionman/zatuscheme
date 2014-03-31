#ifndef S_CLOSURE_HH
#define S_CLOSURE_HH

#include "lisp_ptr.hh"

class SyntacticClosure{
public:
  SyntacticClosure(Env*, Lisp_ptr free_names, Lisp_ptr expr);
  SyntacticClosure(const SyntacticClosure&) = delete;
  SyntacticClosure(SyntacticClosure&&) = delete;

  ~SyntacticClosure() = default;

  SyntacticClosure& operator=(const SyntacticClosure&) = delete;
  SyntacticClosure& operator=(SyntacticClosure&&) = delete;

  Env* env() const { return env_; }
  const Lisp_ptr& free_names() const { return free_names_; }
  const Lisp_ptr& expr() const { return expr_; }

private:
  Env* env_;
  Lisp_ptr free_names_;         // cons list
  Lisp_ptr expr_;
};

bool identifierp(Lisp_ptr);
bool identifier_eq(Env*, Lisp_ptr, Env*, Lisp_ptr);

#endif // S_CLOSURE_HH
