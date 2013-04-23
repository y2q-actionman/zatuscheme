#ifndef ZS_MEMORY_HH
#define ZS_MEMORY_HH

#include <new>
#include "decl.hh"

void* operator new(size_t, Ptr_tag);
void* operator new[](size_t, Ptr_tag) = delete;

void operator delete(void*, Ptr_tag);
void operator delete[](void*, Ptr_tag) = delete;

#endif // ZS_MEMORY_HH
