#ifndef ENV_HH
#define ENV_HH

#include <unordered_map>

#include "lisp_ptr.hh"

class Env {
public:
  typedef std::unordered_map<Symbol*, Lisp_ptr> map_type;

  Env();
  Env(const Env&);
  Env(Env&&);
  Env(Env*);

  ~Env();

  Env& operator=(const Env&);
  Env& operator=(Env&&);

  Lisp_ptr traverse(Symbol*, Lisp_ptr);
  void local_set(Symbol*, Lisp_ptr);
  Env* push();

  int add_ref();
  int release();

private:
  map_type map_;
  Env* next_;
  int refcnt_;
};

#endif // ENV_HH
