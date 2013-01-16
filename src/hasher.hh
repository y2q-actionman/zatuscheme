#ifndef HASHER_HH
#define HASHER_HH

#include <functional>
#include <cstddef>
#include "lisp_ptr.hh"

// 'eq?' semantics
size_t eq_hash(const Lisp_ptr&);

size_t eq_id_hash(const Lisp_ptr&);

// for std::hash interface (eq-id-hash)
namespace std{
  template<> struct hash<Lisp_ptr>;
}

#include "hasher.i.hh"

#endif // HASHER_HH
