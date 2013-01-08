#ifndef ENV_HH
#define ENV_HH

#include <unordered_map>
#include <iosfwd>

#include "lisp_ptr.hh"

class Env {
public:
  typedef std::unordered_map<Symbol*, Lisp_ptr> map_type;

  Env(const Env&) = delete;
  Env(Env&&) = delete;
  Env(Env* e);

  ~Env();

  Env& operator=(const Env&) = delete;
  Env& operator=(Env&&) = delete;

  Lisp_ptr traverse(Symbol*, Lisp_ptr);
  void local_set(Symbol*, Lisp_ptr);
  Env* push();

  Env* fork() const;
  
  // void clear();
  // void clear_all();

  friend std::ostream& operator<<(std::ostream&, const Env&);

  template<typename Fun>
  void visit_map(Fun f){ f(map_); } // TODO: expose the map?

private:
  map_type map_;
  Env* next_;
};

#include "env.i.hh"

#endif // ENV_HH
