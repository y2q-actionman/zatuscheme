#include <climits>

#include "builtin_vector.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

using namespace std;

namespace builtin {

Lisp_ptr vector_make(ZsArgs args){
  check_range(args[0], 0);

  return {zs_new<Vector>(args[0].get<int>(), args[1])};
}

Lisp_ptr vector_vector(ZsArgs args){
  return {zs_new<Vector>(args.begin(), args.end())};
}

Lisp_ptr vector_length(ZsArgs args){
  check_type(Ptr_tag::vector, args[0]);
  auto v = args[0].get<Vector*>();

  // TODO: add range check, and remove cast
  return Lisp_ptr{static_cast<int>(v->size())};
}

Lisp_ptr vector_ref(ZsArgs args){
  check_type(Ptr_tag::vector, args[0]);
  auto v = args[0].get<Vector*>();

  check_range(args[1], 0, v->size());
  auto ind = args[1].get<int>();
  
  return (*v)[ind];
}

Lisp_ptr vector_set(ZsArgs args){
  check_type(Ptr_tag::vector, args[0]);
  auto v = args[0].get<Vector*>();

  check_range(args[1], 0, v->size());
  auto ind = args[1].get<int>();
  
  (*v)[ind] = args[2];
  return args[2];
}

} // namespace builtin
