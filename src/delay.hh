#ifndef DELAY_HH
#define DELAY_HH

#include "lisp_ptr.hh"

class Delay{
public:
  Delay(Lisp_ptr, Env*);
  ~Delay();

  bool forced() const { return forced_; }
  Lisp_ptr get() const { return expr_; }
  Env* env() const { return env_; }

  void force(Lisp_ptr);

private:
  Lisp_ptr expr_;
  bool forced_;
  Env* env_;
};

#endif // DELAY_HH
