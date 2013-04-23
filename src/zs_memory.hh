#ifndef ZS_MEMORY_HH
#define ZS_MEMORY_HH

#include <new>
#include <memory>
#include "decl.hh"

void* operator new(size_t, Ptr_tag);
void* operator new[](size_t, Ptr_tag) = delete;

void operator delete(void*, Ptr_tag);
void operator delete[](void*, Ptr_tag) = delete;

// like 'std::default_delete'
template<typename T>
struct zs_memory_delete{
  void operator()(T* p){
    p->~T();
    operator delete(p, to_tag<T>()); 
  }
};

#endif // ZS_MEMORY_HH
