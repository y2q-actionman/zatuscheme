#ifndef DELAY_HH
#define DELAY_HH

#include "lisp_ptr.hh"

class Delay{
public:
  Delay(Lisp_ptr p, Env* e) : expr_(p), env_(e){}
  Delay(const Delay&) = delete;
  Delay(Delay&&) = delete;

  ~Delay() = default;

  Delay& operator=(const Delay&) = delete;
  Delay& operator=(Delay&&) = delete;

  Lisp_ptr get() const { return expr_; }
  Env* env() const { return env_; }

  bool forced() const { return (env_ == nullptr); }

  void force(Lisp_ptr p){
    expr_ = p;
    env_ = nullptr;
  }

private:
  Lisp_ptr expr_;
  Env* env_;
};

#endif // DELAY_HH
