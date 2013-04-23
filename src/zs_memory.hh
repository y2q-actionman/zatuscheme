#ifndef ZS_MEMORY_HH
#define ZS_MEMORY_HH

#include "decl.hh"

// primitives
void* zs_m_in(void* p, Ptr_tag tag);
void zs_m_out(void* p, Ptr_tag tag);

// wrappers
template<typename T, typename... Args>
T* zs_new(Args...);

template<typename T, Ptr_tag, typename... Args>
T* zs_new_with_tag(Args...);

template<typename T>
void zs_delete(T*);

template<typename T>
struct zs_deleter;

#include "zs_memory.i.hh"

#endif // ZS_MEMORY_HH
