#include "builtin_vector.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "eval.hh"
#include "zs_error.hh"
#include "cons_util.hh"

using namespace std;

namespace {

zs_error vector_type_check_failed(const char* func_name, Lisp_ptr p){
  return zs_error_arg1(func_name,
                       printf_string("arg is not %s!", stringify(Ptr_tag::vector)),
                       {p});
}

} // namespace

namespace builtin {

Lisp_ptr vector_make(ZsArgs args){
  if(args[0].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed("make-vector", Ptr_tag::integer, args[0]);
  }
  auto count = args[0].get<int>();

  switch(args.size()){
  case 1:
    return {new Vector(count, {})};
  case 2:
    return {new Vector(count, args[1])};
  default:
    throw builtin_argcount_failed("make-vector", 1, 2, args.size());
  }
}

Lisp_ptr vector_vector(ZsArgs args){
  return {new Vector(args.begin(), args.end())};
}

Lisp_ptr vector_length(ZsArgs args){
  auto v = args[0].get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector-length", args[0]);
  }

  // TODO: add range check, and remove cast
  return Lisp_ptr{Ptr_tag::integer,
      static_cast<int>(v->size())};
}

Lisp_ptr vector_ref(ZsArgs args){
  auto v = args[0].get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector-ref", args[0]);
  }

  if(args[1].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed("vector-ref", Ptr_tag::integer, args[1]);
  }
  auto ind = args[1].get<int>();

  if(ind < 0 || ind >= static_cast<signed>(v->size())){
    throw zs_error_arg1("vector-ref",
                        printf_string("index is out-of-bound ([0, %ld), supplied %d",
                                      v->size(), ind));
  }

  return (*v)[ind];
}

Lisp_ptr vector_set(ZsArgs args){
  auto v = args[0].get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector-set!", args[0]);
  }

  if(args[1].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed("vector-set!", Ptr_tag::integer, args[1]);
  }
  auto ind = args[1].get<int>();

  if(ind < 0 || ind >= static_cast<signed>(v->size())){
    throw zs_error_arg1("vector-set!",
                        printf_string("index is out-of-bound ([0, %ld), supplied %d",
                                      v->size(), ind));
  }

  (*v)[ind] = args[2];
  return args[2];
}

Lisp_ptr vector_to_list(ZsArgs args){
  auto v = args[0].get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector->list", args[0]);
  }

  return make_cons_list(v->begin(), v->end());
}

Lisp_ptr vector_from_list(ZsArgs args){
  if(args[0].tag() != Ptr_tag::cons){
    throw builtin_type_check_failed("list->vector", Ptr_tag::cons, args[0]);
  }

  return {new Vector(begin(args[0]), end(args[0]))};
}

Lisp_ptr vector_fill(ZsArgs args){
  auto v = args[0].get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector-fill!", args[0]);
  }

  std::fill(v->begin(), v->end(), args[1]);
  return {v};
}

} // namespace builtin
