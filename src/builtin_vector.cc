#include "builtin_vector.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "number.hh"

using namespace std;
using namespace Procedure;

namespace {

void vector_type_check_failed(const char* func_name, Lisp_ptr p){
  builtin_type_check_failed(func_name, Ptr_tag::vector, p);
}

void vector_make(){
  auto arg1 = VM.stack.top();
  VM.stack.pop();

  auto num = arg1.get<Number*>();
  if(!num){
    builtin_type_check_failed("make-vector", Ptr_tag::number, arg1);
    clean_args();
    return;
  }

  if(num->type() != Number::Type::integer){
    fprintf(zs::err, "native func: make-vector: arg's number is not %s! (%s)\n",
            stringify(Number::Type::integer), stringify(num->type()));
    VM.return_value = {};
    clean_args();
    return;
  }
  auto count = num->get<Number::integer_type>();

  Lisp_ptr fill;

  if(VM.stack.top().tag() == Ptr_tag::vm_op){
    VM.stack.pop();
    fill = {};
  }else{
    fill = pick_args_1();
  }

  VM.return_value = {new Vector(count, fill)};
}

void vector_length(){
  auto arg1 = pick_args_1();
  auto v = arg1.get<Vector*>();
  if(!v){
    vector_type_check_failed("vector-length", arg1);
    return;
  }

  VM.return_value = {new Number(static_cast<Number::integer_type>(v->size()))};
}

void vector_ref(){
  auto arg = pick_args<2>();
  auto v = arg[0].get<Vector*>();
  if(!v){
    vector_type_check_failed("vector-ref", arg[0]);
    return;
  }

  auto num = arg[1].get<Number*>();
  if(!num){
    builtin_type_check_failed("vector-ref", Ptr_tag::number, arg[1]);
    return;
  }

  if(num->type() != Number::Type::integer){
    fprintf(zs::err, "native func: vector-ref: arg's number is not %s! (%s)\n",
            stringify(Number::Type::integer), stringify(num->type()));
    VM.return_value = {};
    return;
  }
  auto ind = num->get<Number::integer_type>();

  if(ind < 0 || ind >= v->size()){
    fprintf(zs::err, "native func: vector-ref: index is out-of-bound ([0, %ld), supplied %ld\n",
            v->size(), ind);
    VM.return_value = {};
    return;
  }

  VM.return_value = (*v)[ind];
}

void vector_set(){
  auto arg = pick_args<3>();
  auto v = arg[0].get<Vector*>();
  if(!v){
    vector_type_check_failed("vector-set!", arg[0]);
    return;
  }

  auto num = arg[1].get<Number*>();
  if(!num){
    builtin_type_check_failed("vector-set!", Ptr_tag::number, arg[1]);
    return;
  }

  if(num->type() != Number::Type::integer){
    fprintf(zs::err, "native func: vector-set!: arg's number is not %s! (%s)\n",
            stringify(Number::Type::integer), stringify(num->type()));
    VM.return_value = {};
    return;
  }
  auto ind = num->get<Number::integer_type>();

  if(ind < 0 || ind >= v->size()){
    fprintf(zs::err, "native func: vector-set!: index is out-of-bound ([0, %ld), supplied %ld\n",
            v->size(), ind);
    VM.return_value = {};
    return;
  }

  (*v)[ind] = arg[2];
  VM.return_value = arg[2];
}

void vector_to_list(){
  auto arg1 = pick_args_1();
  auto v = arg1.get<Vector*>();
  if(!v){
    vector_type_check_failed("vector->list", arg1);
    return;
  }

  VM.return_value = make_cons_list(v->begin(), v->end());
}

void vector_from_list(){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::cons){
    builtin_type_check_failed("list->vector", Ptr_tag::cons, arg);
    return;
  }

  Vector ret;
  
  do_list(arg,
          [&](Cons* c) -> bool{
            ret.push_back(c->car());
            return true;
          },
          [](Lisp_ptr){});

  VM.return_value = {new Vector(std::move(ret))};
}

void vector_fill(){
  auto arg = pick_args<2>();
  auto v = arg[0].get<Vector*>();
  if(!v){
    vector_type_check_failed("vector-fill!", arg[0]);
    return;
  }

  std::fill(v->begin(), v->end(), arg[1]);
  VM.return_value = {v};
}


} // namespace

const BuiltinFunc
builtin_vector[] = {
  {"vector?", {
      type_check_pred<Ptr_tag::vector>,
      {Calling::function, 1}}},
  {"make-vector", {
      vector_make,
      {Calling::function, 1, Variadic::t}}},
  {"vector", {
      procedure_vector, 
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