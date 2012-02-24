#ifndef ENV_HH
#define ENV_HH

#include <unordered_map>

#include "lisp_ptr.hh"

class Env {
public:
  Env() = default;
  
  Env(const Env&) = default;
  Env(Env&&) = default;

  ~Env() = default;

  Env& operator=(const Env&) = default;
  Env& operator=(Env&&) = default;


  Lisp_ptr find(Symbol*) const;
  Lisp_ptr set(Symbol*, Lisp_ptr);

private:
  std::unordered_map<Symbol*, Lisp_ptr> map_;
};

#endif // ENV_HH
