#include <array>

#include "builtin.hh"
#include "number.hh"
#include "function.hh"
#include "lisp_ptr.hh"
#include "eval.hh"

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


static struct Entry {
  const char* name;
  Function func;
} 
builtin_func[] = {
  // syntaxes
  {"quote", Function{
      whole_function_quote,
      Function::Calling::whole_function, {1, false}}},
  {"set!", Function{
      whole_function_set,
      Function::Calling::whole_function, {2, false}}},
  {"begin", Function{
      whole_function_begin,
      Function::Calling::whole_function, {1, true}}},

  {"cond", Function{
      whole_function_unimplemented,
      Function::Calling::whole_function, {0, true}}},
  {"case", Function{
      whole_function_unimplemented,
      Function::Calling::whole_function, {0, true}}},
  {"and", Function{
      whole_function_unimplemented,
      Function::Calling::whole_function, {0, true}}},
  {"or", Function{
      whole_function_unimplemented,
      Function::Calling::whole_function, {0, true}}},
  {"let", Function{
      whole_function_unimplemented,
      Function::Calling::whole_function, {0, true}}},
  {"let*", Function{
      whole_function_unimplemented,
      Function::Calling::whole_function, {0, true}}},
  {"letrec", Function{
      whole_function_unimplemented,
      Function::Calling::whole_function, {0, true}}},
  {"do", Function{
      whole_function_unimplemented,
      Function::Calling::whole_function, {0, true}}},
  {"delay", Function{
      whole_function_unimplemented,
      Function::Calling::whole_function, {0, true}}},

  {"unquote", Function{
      whole_function_pass_through,
      Function::Calling::whole_function, {0, true}}},
  {"unquote-splicing", Function{
      whole_function_pass_through,
      Function::Calling::whole_function, {0, true}}},
  {"eval", Function{
      whole_function_error,
      Function::Calling::whole_function, {0, false}}},
  {"=>", Function{
      whole_function_error,
      Function::Calling::whole_function, {0, false}}},

  // functions
  {"+", Function{
      plus_2,
      Function::Calling::function, {2, true}}},
  {"list", Function{
      [](){stack_to_list(false);}, 
      Function::Calling::function, {1, true}}},
  {"list*", Function{
      [](){stack_to_list(true);}, 
      Function::Calling::function, {1, true}}},
  {"vector", Function{
      stack_to_vector, 
      Function::Calling::function, {1, true}}},
  {"boolean?", Function{
      type_check_pred<Ptr_tag::boolean>, 
      Function::Calling::function, {1, false}}},
  {"symbol?", Function{
      type_check_pred<Ptr_tag::symbol>,
      Function::Calling::function, {1, false}}},
  {"char?", Function{
      type_check_pred<Ptr_tag::character>,
      Function::Calling::function, {1, false}}},
  {"vector?", Function{
      type_check_pred<Ptr_tag::vector>,
      Function::Calling::function, {1, false}}},
  {"procedure?", Function{
      type_check_pred<Ptr_tag::function>,
      Function::Calling::function, {1, false}}},
  {"pair?", Function{
      [](){
        auto args = pick_args<1>();
        VM.return_value() = Lisp_ptr{(args[0].tag() == Ptr_tag::cons) && !nullp(args[0])};
      },
      Function::Calling::function, {1, false}}},
  {"number?", Function{
      type_check_pred<Ptr_tag::number>,
      Function::Calling::function, {1, false}}},
  {"string?", Function{
      type_check_pred<Ptr_tag::string>,
      Function::Calling::function, {1, false}}},
  {"port?", Function{
      type_check_pred<Ptr_tag::port>,
      Function::Calling::function, {1, false}}},
  {"eql", Function{
      eql,
      Function::Calling::function, {2, false}}},
  {"eq", Function{
      eq,
      Function::Calling::function, {2, false}}}
};

void install_builtin(){
  for(auto& e : builtin_func){
    Symbol* s = VM.symtable.intern(e.name);
    VM.set(s, Lisp_ptr{&e.func});
  }
}
