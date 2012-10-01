#ifndef ENV_I_HH
#define ENV_I_HH

#ifndef ENV_HH
#error "Please include via parent file"
#endif

inline int Env::add_ref(){
  return ++refcnt_;
}

inline int Env::release(){
  return --refcnt_;
}

#endif // ENV_I_HH
