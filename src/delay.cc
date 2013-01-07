#include <cassert>

#include "delay.hh"

Delay::Delay(Lisp_ptr p, Env* e)
  : expr_(p), env_(e){
  assert(e);
}

Delay::~Delay() = default;

void Delay::force(Lisp_ptr p){
  expr_ = p;
  env_ = nullptr;
}
