#include "s_closure.hh"

SyntacticClosure::SyntacticClosure(Env* e, Cons* f, Lisp_ptr ex)
  : env_(e), free_names_(f), expr_(ex){}

SyntacticClosure::~SyntacticClosure() = default;
