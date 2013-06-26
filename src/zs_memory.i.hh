#ifndef ZS_MEMORY_I_HH
#define ZS_MEMORY_I_HH

#ifndef ZS_MEMORY_HH
#error "Please include via parent file"
#endif

#include <utility>

template<typename T, typename... Args>
T* zs_new(Args&&... args){
  return zs_new_with_tag<T, to_tag<T*>()>(std::forward<Args>(args)...);
}

template<typename T, Ptr_tag tag, typename... Args>
T* zs_new_with_tag(Args&&... args){
  auto p = new T(std::forward<Args>(args)...);
  return static_cast<T*>(zs_m_in(p, tag));
}

template<typename T>
void zs_delete(T* p){
  zs_m_out(p);
  delete p;
}

#endif // ZS_MEMORY_I_HH
