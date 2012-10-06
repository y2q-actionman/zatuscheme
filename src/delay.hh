#ifndef DELAY_HH
#define DELAY_HH

#include "lisp_ptr.hh"

class Delay{
public:
  Delay(Lisp_ptr p) : expr_(p), forced_(false){}

  bool forced() const { return forced_; }
  Lisp_ptr get() const { return expr_; }

  void force(Lisp_ptr p){
    expr_ = p;
    forced_ = true;
  }

private:
  Lisp_ptr expr_;
  bool forced_;
};

#endif // DELAY_HH
