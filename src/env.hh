#ifndef ENV_HH
#define ENV_HH

#include <unordered_map>

#include "lisp_ptr.hh"

class Env {
public:
  typedef std::unordered_map<Symbol*, Lisp_ptr> map_type;

  Env(const Env&) = delete;
  Env(Env&&) = delete;
  Env(Env* e = nullptr);

  ~Env();

  Env& operator=(const Env&) = delete;
  Env& operator=(Env&&) = delete;

  // void steal(Env*);

  Lisp_ptr traverse(Symbol*, Lisp_ptr);
  void local_set(Symbol*, Lisp_ptr);
  Env* push();
  
  inline int add_ref();
  inline int release();

private:
  map_type map_;
  Env* next_;
  int refcnt_;
};

#include "env.i.hh"

#endif // ENV_HH
