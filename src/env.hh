#ifndef ENV_HH
#define ENV_HH

#include <iosfwd>
#include <unordered_map>

#include "equality.hh"
#include "lisp_ptr.hh"

class Env {
public:
  typedef std::unordered_map<Lisp_ptr, Lisp_ptr, EqHashObj, EqObj>
    map_type;

  Env(const Env&) = delete;
  Env(Env&&) = delete;
  Env(Env* e);

  ~Env();

  Env& operator=(const Env&) = delete;
  Env& operator=(Env&&) = delete;

  bool is_bound(Lisp_ptr);
  Lisp_ptr find(Lisp_ptr);
  void set(Lisp_ptr, Lisp_ptr);
  void local_set(Lisp_ptr, Lisp_ptr);
  Env* push();

  Env* fork() const;
  
  friend std::ostream& operator<<(std::ostream&, const Env&);
  friend void gc_mark(Env*);

private:
  template<typename Fun>
  Lisp_ptr traverse(Lisp_ptr, Fun);

  map_type map_;
  Env* next_;
};

#endif // ENV_HH
