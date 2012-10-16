#include "delay.hh"
#include "env.hh"

Delay::Delay(Lisp_ptr p, Env* e)
  : expr_(p), forced_(false), env_(e)
{
  if(env_) add_ref(env_);
}

Delay::~Delay(){
  if(env_) release(env_);
}

void Delay::force(Lisp_ptr p){
  expr_ = p;
  forced_ = true;
  if(env_) release(env_);
  env_ = nullptr;
}

