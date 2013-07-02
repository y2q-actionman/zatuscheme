#ifndef ENV_HH
#define ENV_HH

#include <iosfwd>
#include <unordered_map>
#include <utility>

#include "equality.hh"
#include "lisp_ptr.hh"

class Env {
public:
  typedef std::unordered_map<Lisp_ptr, Lisp_ptr, EqHashObj, EqObj>
    map_type;

  Env(const Env&) = delete;
  Env(Env&&) = delete;
  explicit Env(Env* e);

  ~Env();

  Env& operator=(const Env&) = delete;
  Env& operator=(Env&&) = delete;

  std::pair<Lisp_ptr, bool> find(Lisp_ptr) const;
  void set(Lisp_ptr, Lisp_ptr);
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
