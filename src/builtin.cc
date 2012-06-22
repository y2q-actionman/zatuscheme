#include <array>

#include "builtin.hh"
#include "number.hh"
#include "procedure.hh"
#include "lisp_ptr.hh"
#include "eval.hh"

using namespace std;
using namespace Procedure;

namespace {

template<int i>
array<Lisp_ptr, i> pick_args(){
  auto ret = array<Lisp_ptr, i>();

  for(auto it = ret.rbegin(); it != ret.rend(); ++it){
    *it = VM.stack().top();
    VM.stack().pop();
  }

  VM.stack().pop(); // kill arg_bottom

  return ret;
}

} // namespace

static void plus_2(){
  auto args = pick_args<2>();

  VM.return_value() = {};

  Number* n1 = args[0].get<Number*>();
  if(!n1){
    fprintf(stderr, "native func '+': first arg is not number! %s\n",
            stringify(args[0].tag()));
    return;
  }

  Number* n2 = args[1].get<Number*>();
  if(!n2){
    fprintf(stderr, "native func '+': second arg is not number! %s\n",
            stringify(args[1].tag()));
    return;
  }

  Number* newn = new Number(n1->get<long>() + n2->get<long>());
  VM.return_value() = Lisp_ptr(newn);
}

void stack_to_list(bool dot_list){
  Cons* c = new Cons;
  Cons* prev_c = c;
  Lisp_ptr ret = Lisp_ptr{c};

  while(1){
    c->rplaca(VM.stack().top());
    VM.stack().pop();

    if(VM.stack().top().tag() == Ptr_tag::vm_op){
      VM.stack().pop();
      break;
    }

    Cons* newc = new Cons;
    c->rplacd(Lisp_ptr(newc));
    prev_c = c;
    c = newc;
  }

  if(dot_list){
    if(c != prev_c){
      prev_c->rplacd(c->car());
    }else{
      ret = c->car();
    }
    delete c;
  }else{
    c->rplacd(Cons::NIL);
  }

  VM.return_value() = Lisp_ptr{ret};
}

void stack_to_vector(){
  auto v = new Vector;

  while(1){
    v->push_back(VM.stack().top());
    VM.stack().pop();

    if(VM.stack().top().tag() == Ptr_tag::vm_op){
      VM.stack().pop();
      break;
    }
  }

  VM.return_value() = Lisp_ptr{v};
}

template <Ptr_tag p>
static
void type_check_pred(){
  auto args = pick_args<1>();
  VM.return_value() = Lisp_ptr{args[0].tag() == p};
}

static
void type_check_pair(){
  auto args = pick_args<1>();
  VM.return_value() = Lisp_ptr{(args[0].tag() == Ptr_tag::cons) && !nullp(args[0])};
}

static
void type_check_procedure(){
  auto args = pick_args<1>();
  VM.return_value() = Lisp_ptr{(args[0].tag() == Ptr_tag::i_procedure)
                               || (args[0].tag() == Ptr_tag::i_procedure)};
}

static bool eq_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() != b.tag()) return false;

  if(a.tag() == Ptr_tag::boolean){
    return a.get<bool>() == b.get<bool>();
  }else if(a.tag() == Ptr_tag::character){
    return a.get<char>() == b.get<char>();
  }else{
    return a.get<void*>() == b.get<void*>();
  }
}

static
void list(){
  stack_to_list(false);
}

static
void list_star(){
  stack_to_list(true);
}
 
void eq(){
  auto args = pick_args<2>();
  
  VM.return_value() = Lisp_ptr{eq_internal(args[0], args[1])};
}

void eql(){
  auto args = pick_args<2>();

  if(eq_internal(args[0], args[1])){
    VM.return_value() = Lisp_ptr{true};
    return;
  }
    
  if(args[0].tag() == Ptr_tag::number && args[1].tag() == Ptr_tag::number){
    VM.return_value() = Lisp_ptr{eql(*args[0].get<Number*>(), *args[1].get<Number*>())};
    return;
  }

  VM.return_value() = Lisp_ptr{false};
  return;
}

static constexpr struct Entry {
  const char* name;
  const NProcedure func;

  constexpr Entry(const char* n, const NProcedure& f)
    : name(n), func(f){}
} builtin_func[] = {
  // syntaxes
  {"quote", {
      whole_function_quote,
      Calling::whole_function, {1, false}}},
  {"lambda", {
      whole_function_lambda,
      Calling::whole_function, {1, true}}},
  {"if", {
      whole_function_if,
      Calling::whole_function, {3, false}}},
  {"set!", {
      whole_function_set,
      Calling::whole_function, {2, false}}},
  {"define", {
      whole_function_define,
      Calling::whole_function, {2, true}}},
  {"quasiquote", {
      whole_function_quasiquote,
      Calling::whole_function, {1, false}}},
  {"begin", {
      whole_function_begin,
      Calling::whole_function, {1, true}}},

  {"cond", {
      whole_function_unimplemented,
      Calling::whole_function, {0, true}}},
  {"case", {
      whole_function_unimplemented,
      Calling::whole_function, {0, true}}},
  {"and", {
      whole_function_unimplemented,
      Calling::whole_function, {0, true}}},
  {"or", {
      whole_function_unimplemented,
      Calling::whole_function, {0, true}}},
  {"let", {
      whole_function_unimplemented,
      Calling::whole_function, {0, true}}},
  {"let*", {
      whole_function_unimplemented,
      Calling::whole_function, {0, true}}},
  {"letrec", {
      whole_function_unimplemented,
      Calling::whole_function, {0, true}}},
  {"do", {
      whole_function_unimplemented,
      Calling::whole_function, {0, true}}},
  {"delay", {
      whole_function_unimplemented,
      Calling::whole_function, {0, true}}},

  {"unquote", {
      whole_function_pass_through,
      Calling::whole_function, {0, true}}},
  {"unquote-splicing", {
      whole_function_pass_through,
      Calling::whole_function, {0, true}}},
  {"else", {
      whole_function_error,
      Calling::whole_function, {0, false}}},
  {"=>", {
      whole_function_error,
      Calling::whole_function, {0, false}}},

  // functions
  {"+", {
      plus_2,
      Calling::function, {2, true}}},
  {"list", {
      list,
      Calling::function, {1, true}}},
  {"list*", {
      list_star,
      Calling::function, {1, true}}},
  {"vector", {
      stack_to_vector, 
      Calling::function, {1, true}}},
  {"boolean?", {
      type_check_pred<Ptr_tag::boolean>, 
      Calling::function, {1, false}}},
  {"symbol?", {
      type_check_pred<Ptr_tag::symbol>,
      Calling::function, {1, false}}},
  {"char?", {
      type_check_pred<Ptr_tag::character>,
      Calling::function, {1, false}}},
  {"vector?", {
      type_check_pred<Ptr_tag::vector>,
      Calling::function, {1, false}}},
  {"procedure?", {
      type_check_procedure,
      Calling::function, {1, false}}},
  {"pair?", {
      type_check_pair,
      Calling::function, {1, false}}},
  {"number?", {
      type_check_pred<Ptr_tag::number>,
      Calling::function, {1, false}}},
  {"string?", {
      type_check_pred<Ptr_tag::string>,
      Calling::function, {1, false}}},
  {"port?", {
      type_check_pred<Ptr_tag::port>,
      Calling::function, {1, false}}},
  {"eql", {
      eql,
      Calling::function, {2, false}}},
  {"eq", {
      eq,
      Calling::function, {2, false}}}
};

void install_builtin(){
  for(auto& e : builtin_func){
    VM.set(VM.symtable.intern(e.name), Lisp_ptr{&e.func});
  }
}
