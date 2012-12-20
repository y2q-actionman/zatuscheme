#include "builtin_vector.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "number.hh"
#include "eval.hh"
#include "util.hh"

using namespace std;

namespace {

zs_error vector_type_check_failed(const char* func_name, Lisp_ptr p){
  return make_zs_error("native func: %s: arg is not %s! (%s)\n",
                       func_name, stringify(Ptr_tag::vector), stringify(p.tag()));
}

} // namespace

Lisp_ptr vector_make(){
  ZsArgs args;

  auto num = args[0].get<Number*>();
  if(!num){
    throw builtin_type_check_failed("make-vector", Ptr_tag::number, args[0]);
  }

  if(num->type() != Number::Type::integer){
    throw make_zs_error("native func: make-vector: arg's number is not %s! (%s)\n",
                        stringify(Number::Type::integer), stringify(num->type()));
  }
  auto count = num->get<Number::integer_type>();

  switch(args.size()){
  case 1:
    return {new Vector(count, {})};
  case 2:
    return {new Vector(count, args[1])};
  default:
    throw builtin_argcount_failed("make-vector", 1, 2, args.size());
  }
}

Lisp_ptr vector_vector(){
  ZsArgs args;
  return {new Vector(args.begin(), args.end())};
}

Lisp_ptr vector_length(){
  ZsArgs args{1};

  auto v = args[0].get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector-length", args[0]);
  }

  return {new Number(static_cast<Number::integer_type>(v->size()))};
}

Lisp_ptr vector_ref(){
  ZsArgs args{2};

  auto v = args[0].get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector-ref", args[0]);
  }

  auto num = args[1].get<Number*>();
  if(!num){
    throw builtin_type_check_failed("vector-ref", Ptr_tag::number, args[1]);
  }

  if(num->type() != Number::Type::integer){
    throw make_zs_error("native func: vector-ref: arg's number is not %s! (%s)\n",
                        stringify(Number::Type::integer), stringify(num->type()));
  }
  auto ind = num->get<Number::integer_type>();

  if(ind < 0 || ind >= v->size()){
    throw make_zs_error("native func: vector-ref: index is out-of-bound ([0, %ld), supplied %ld\n",
                        v->size(), ind);
  }

  return (*v)[ind];
}

Lisp_ptr vector_set(){
  ZsArgs args{3};

  auto v = args[0].get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector-set!", args[0]);
  }

  auto num = args[1].get<Number*>();
  if(!num){
    throw builtin_type_check_failed("vector-set!", Ptr_tag::number, args[1]);
  }

  if(num->type() != Number::Type::integer){
    throw make_zs_error("native func: vector-set!: arg's number is not %s! (%s)\n",
                        stringify(Number::Type::integer), stringify(num->type()));
  }
  auto ind = num->get<Number::integer_type>();

  if(ind < 0 || ind >= v->size()){
    throw make_zs_error("native func: vector-set!: index is out-of-bound ([0, %ld), supplied %ld\n",
                        v->size(), ind);
  }

  (*v)[ind] = args[2];
  return args[2];
}

Lisp_ptr vector_to_list(){
  ZsArgs args{1};

  auto v = args[0].get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector->list", args[0]);
  }

  return make_cons_list(v->begin(), v->end());
}

Lisp_ptr vector_from_list(){
  ZsArgs args{1};

  if(args[0].tag() != Ptr_tag::cons){
    throw builtin_type_check_failed("list->vector", Ptr_tag::cons, args[0]);
  }

  return {new Vector(begin(args[0]), end(args[0]))};
}

Lisp_ptr vector_fill(){
  ZsArgs args{2};

  auto v = args[0].get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector-fill!", args[0]);
  }

  std::fill(v->begin(), v->end(), args[1]);
  return {v};
}
