#include "builtin_vector.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

using namespace std;

namespace builtin {

Lisp_ptr internal_vector_make(ZsArgs args){
  if(args[0].tag() != Ptr_tag::integer){
    throw_builtin_type_check_failed(Ptr_tag::integer, args[0]);
  }

  auto count = args[0].get<int>();
  if(count < 0){
    throw_zs_error(args[0], "passed size is invalid");
  }    

  return {zs_new<Vector>(count, args[1])};
}

Lisp_ptr vector_length(ZsArgs args){
  auto v = args[0].get<Vector*>();
  if(!v){
    throw_builtin_type_check_failed(Ptr_tag::vector, args[0]);
  }

  // TODO: add range check, and remove cast
  return Lisp_ptr{static_cast<int>(v->size())};
}

Lisp_ptr vector_ref(ZsArgs args){
  auto v = args[0].get<Vector*>();
  if(!v){
    throw_builtin_type_check_failed(Ptr_tag::vector, args[0]);
  }

  if(args[1].tag() != Ptr_tag::integer){
    throw_builtin_type_check_failed(Ptr_tag::integer, args[1]);
  }
  auto ind = args[1].get<int>();

  if(ind < 0 || ind >= static_cast<signed>(v->size())){
    throw_builtin_range_check_failed(v->size(), ind);
  }

  return (*v)[ind];
}

Lisp_ptr vector_set(ZsArgs args){
  auto v = args[0].get<Vector*>();
  if(!v){
    throw_builtin_type_check_failed(Ptr_tag::vector, args[0]);
  }

  if(args[1].tag() != Ptr_tag::integer){
    throw_builtin_type_check_failed(Ptr_tag::integer, args[1]);
  }
  auto ind = args[1].get<int>();

  if(ind < 0 || ind >= static_cast<signed>(v->size())){
    throw_builtin_range_check_failed(v->size(), ind);
  }

  (*v)[ind] = args[2];
  return args[2];
}

} // namespace builtin
