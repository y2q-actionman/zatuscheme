#include "delay.hh"
#include "env.hh"

Delay::Delay(Lisp_ptr p, Env* e)
  : expr_(p), forced_(false), env_(e)
{
}

Delay::~Delay(){
}

void Delay::force(Lisp_ptr p){
  expr_ = p;
  forced_ = true;
  env_ = nullptr;
}
