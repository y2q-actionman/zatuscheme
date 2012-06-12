#include <array>

#include "builtin.hh"
#include "number.hh"
#include "function.hh"
#include "lisp_ptr.hh"

using namespace std;

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


void plus_2(){
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
  Cons* ret = new Cons;
  Cons *c = ret, *prev_c = ret;

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
    prev_c->rplacd(c->car());
    delete c;
  }else{
    c->rplacd(Cons::NIL);
  }

  VM.return_value() = Lisp_ptr{ret};
}

void list(){
  stack_to_list(false);
}

void list_star(){
  stack_to_list(true);
}

template <Ptr_tag p>
void type_check_pred(){
  auto args = pick_args<1>();
  VM.return_value() = {args[0].tag() == p};
}  

static bool eq_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() != b.tag()) return false;

  switch(a.tag()){
   case Ptr_tag::boolean:
    return a.get<bool>() == b.get<bool>();
   case Ptr_tag::character:
    return a.get<char>() == b.get<char>();
   default:
    return a.get<void*>() == b.get<void*>();
   }
 }
 
void eq(){
  auto args = pick_args<2>();
  
  VM.return_value() = {eq_internal(args[0], args[1])};
}

void eql(){
  auto args = pick_args<2>();

  if(eq_internal(args[0], args[1])){
    VM.return_value() = {true};
    return;
  }
    
  if(args[0].tag() == Ptr_tag::number && args[1].tag() == Ptr_tag::number){
    VM.return_value() = {eql(*args[0].get<Number*>(), *args[1].get<Number*>())};
    return;
  }

  VM.return_value() = {false};
  return;
}


static struct Entry {
  const char* name;
  Function func;
} 
builtin_func[] = {
  {"+", Function{plus_2, {{}, 2, true}}},
  {"list", Function{list, {{}, 1, true}}},
  {"list*", Function{list_star, {{}, 2, true}}},
  {"boolean?", Function{type_check_pred<Ptr_tag::boolean>, {{}, 1, false}}},
  {"symbol?", Function{type_check_pred<Ptr_tag::symbol>, {{}, 1, false}}},
  {"char?", Function{type_check_pred<Ptr_tag::character>, {{}, 1, false}}},
  {"vector?", Function{type_check_pred<Ptr_tag::vector>, {{}, 1, false}}},
  {"procedure?", Function{type_check_pred<Ptr_tag::function>, {{}, 1, false}}},
  // {"pair?", Function{type_check_pred<Ptr_tag::cons>, {{}, 1, false}}}, // this evaluates nil is #t!
  {"number?", Function{type_check_pred<Ptr_tag::number>, {{}, 1, false}}},
  {"string?", Function{type_check_pred<Ptr_tag::string>, {{}, 1, false}}},
  {"port?", Function{type_check_pred<Ptr_tag::port>, {{}, 1, false}}},
  {"eql", Function{eql, {{}, 2, false}}},
  {"eq", Function{eq, {{}, 2, false}}}
};

void install_builtin(){
  for(auto& e : builtin_func){
    Symbol* s = VM.symtable.intern(e.name);
    VM.set(s, Lisp_ptr{&e.func});
  }
}
