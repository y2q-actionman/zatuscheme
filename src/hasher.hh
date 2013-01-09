#ifndef HASHER_HH
#define HASHER_HH

#include <functional>

// 'eq?' semantics
namespace std{
  template<> struct hash<Lisp_ptr>;
}

#include "hasher.i.hh"

#endif // HASHER_HH
