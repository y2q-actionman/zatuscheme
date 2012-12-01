#include "builtin_vector.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "number.hh"
#include "eval.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

namespace {

zs_error vector_type_check_failed(const char* func_name, Lisp_ptr p){
  return make_zs_error("native func: %s: arg is not %s! (%s)\n",
                       func_name, stringify(Ptr_tag::vector), stringify(p.tag()));
}

Lisp_ptr vector_make(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(vm.stack, args);

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
    throw builtin_variadic_argcount_failed("make-vector", 2);
  }
}

Lisp_ptr vector_vector(){
  auto v = new Vector;
  stack_to_vector(vm.stack, *v);
  return v;
}

Lisp_ptr vector_length(){
  auto arg1 = pick_args_1();
  auto v = arg1.get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector-length", arg1);
  }

  return {new Number(static_cast<Number::integer_type>(v->size()))};
}

Lisp_ptr vector_ref(){
  auto arg = pick_args<2>();
  auto v = arg[0].get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector-ref", arg[0]);
  }

  auto num = arg[1].get<Number*>();
  if(!num){
    throw builtin_type_check_failed("vector-ref", Ptr_tag::number, arg[1]);
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
  auto arg = pick_args<3>();
  auto v = arg[0].get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector-set!", arg[0]);
  }

  auto num = arg[1].get<Number*>();
  if(!num){
    throw builtin_type_check_failed("vector-set!", Ptr_tag::number, arg[1]);
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

  (*v)[ind] = arg[2];
  return arg[2];
}

Lisp_ptr vector_to_list(){
  auto arg1 = pick_args_1();
  auto v = arg1.get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector->list", arg1);
  }

  return make_cons_list(v->begin(), v->end());
}

Lisp_ptr vector_from_list(){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::cons){
    throw builtin_type_check_failed("list->vector", Ptr_tag::cons, arg);
  }

  return {new Vector(begin(arg), end(arg))};
}

Lisp_ptr vector_fill(){
  auto arg = pick_args<2>();
  auto v = arg[0].get<Vector*>();
  if(!v){
    throw vector_type_check_failed("vector-fill!", arg[0]);
  }

  std::fill(v->begin(), v->end(), arg[1]);
  return {v};
}


} // namespace

const BuiltinFunc
builtin_vector[] = {
  {"vector?", {
      type_check_pred<Ptr_tag::vector>,
      {Calling::function, 1}}},
  {"make-vector", {
      vector_make,
      {Calling::function, 1, 2}}},
  {"vector", {
      vector_vector, 
      {Calling::function, 1, Variadic::t}}},
  {"vector-length", {
      vector_length,
      {Calling::function, 1}}},
  {"vector-ref", {
      vector_ref,
      {Calling::function, 2}}},
  {"vector-set!", {
      vector_set,
      {Calling::function, 3}}},

  {"vector->list", {
      vector_to_list,
      {Calling::function, 1}}},
  {"list->vector", {
      vector_from_list,
      {Calling::function, 1}}},

  {"vector-fill!", {
      vector_fill,
      {Calling::function, 2}}}
};

const size_t builtin_vector_size = sizeof(builtin_vector) / sizeof(builtin_vector[0]);
