#ifndef ENV_I_HH
#define ENV_I_HH

#ifndef ENV_HH
#error "Please include via parent file"
#endif

inline int add_ref(Env* e){
  return ++(e->refcnt_);
}

inline int release(Env* e){
  auto ret = --(e->refcnt_);
  if(e->refcnt_ <= 0) delete e;
  return ret;
}

#endif // ENV_I_HH
