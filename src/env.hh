#ifndef ENV_HH
#define ENV_HH

#include <unordered_map>
#include <iosfwd>

#include "lisp_ptr.hh"
#include "equality.hh"

class Env {
public:
  typedef std::unordered_map<Lisp_ptr, Lisp_ptr,
                             eq_hash_obj, eq_obj>
    map_type;

  Env(const Env&) = delete;
  Env(Env&&) = delete;
  Env(Env* e);

  ~Env();

  Env& operator=(const Env&) = delete;
  Env& operator=(Env&&) = delete;

  Lisp_ptr find(Lisp_ptr);
  Lisp_ptr set(Lisp_ptr, Lisp_ptr);
  void local_set(Lisp_ptr, Lisp_ptr);
  Env* push();

  Env* fork() const;
  
  friend std::ostream& operator<<(std::ostream&, const Env&);
  friend void gc_mark(Env*);

private:
  map_type map_;
  Env* next_;
};

#endif // ENV_HH
