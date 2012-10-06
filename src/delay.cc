#include "delay.hh"
#include "env.hh"

Delay::Delay(Lisp_ptr p, Env* e)
  : expr_(p), forced_(false), env_(e)
{
  if(env_) env_->add_ref();
}

Delay::~Delay(){
  if(env_) env_->release();
}

void Delay::force(Lisp_ptr p){
  expr_ = p;
  forced_ = true;
  if(env_) env_->release();
  env_ = nullptr;
}

