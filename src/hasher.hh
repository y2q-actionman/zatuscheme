#ifndef HASHER_HH
#define HASHER_HH

#include <functional>
#include <cstddef>
#include "lisp_ptr.hh"

// 'eq?' semantics
size_t eq_hash(const Lisp_ptr&);
size_t eq_id_hash(const Lisp_ptr&);

// function object
struct eq_hash_obj;
struct eq_id_hash_obj;


#include "hasher.i.hh"

#endif // HASHER_HH
