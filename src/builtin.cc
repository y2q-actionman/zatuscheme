#include <array>

#include "builtin.hh"
#include "number.hh"
#include "procedure.hh"
#include "lisp_ptr.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "printer.hh"

using namespace std;
using namespace Procedure;

namespace {

void plus_2(){
  auto args = pick_args<2>();

  VM.return_value = {};

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
  VM.return_value = Lisp_ptr(newn);
}

template <Ptr_tag p>
void type_check_pred(){
  auto arg = pick_args_1();
  VM.return_value = Lisp_ptr{arg.tag() == p};
}

void type_check_pair(){
  auto arg = pick_args_1();
  VM.return_value = Lisp_ptr{(arg.tag() == Ptr_tag::cons) && !nullp(arg)};
}

void type_check_procedure(){
  auto arg = pick_args_1();
  VM.return_value = Lisp_ptr{(arg.tag() == Ptr_tag::i_procedure)
                             || (arg.tag() == Ptr_tag::i_procedure)};
}

Lisp_ptr whole_macro_or_expand(Cons* c){
  if(!c->cdr() || nullp(c->cdr())){
    return c->car();
  }

  const auto if_sym = intern(VM.symtable, "if");
  auto else_clause = whole_macro_or_expand(c->cdr().get<Cons*>());

  return Lisp_ptr(new Cons(Lisp_ptr(if_sym),
         Lisp_ptr(new Cons(c->car(),
         Lisp_ptr(new Cons(Lisp_ptr(vm_op_nop),
         Lisp_ptr(new Cons(else_clause, Cons::NIL))))))));
}

Lisp_ptr whole_macro_and_expand(Cons* c){
  if(!c->cdr() || nullp(c->cdr())){
    return c->car();
  }

  const auto if_sym = intern(VM.symtable, "if");
  auto then_clause = whole_macro_and_expand(c->cdr().get<Cons*>());

  return Lisp_ptr(new Cons(Lisp_ptr(if_sym),
         Lisp_ptr(new Cons(c->car(),
         Lisp_ptr(new Cons(then_clause,
         Lisp_ptr(new Cons(Lisp_ptr(vm_op_nop), Cons::NIL))))))));
}

template<bool default_value, typename Expander>
inline
void whole_macro_andor(Expander e){
  auto arg = pick_args_1();
  if(!arg) return;

  Cons* head;

  int len = bind_cons_list(arg,
                           [](Cons*){},
                           [&](Cons* c){
                             head = c;
                           });
  if(len < 2){
    VM.return_value = Lisp_ptr(default_value);
    return;
  }

  VM.return_value = e(head);
}

void whole_macro_and(){
  whole_macro_andor<true>(whole_macro_and_expand);
}
                 
void whole_macro_or(){
  whole_macro_andor<false>(whole_macro_or_expand);
}

bool eq_internal(Lisp_ptr a, Lisp_ptr b){
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
  
  VM.return_value = Lisp_ptr{eq_internal(args[0], args[1])};
}

void eql(){
  auto args = pick_args<2>();

  if(args[0].tag() == Ptr_tag::number && args[1].tag() == Ptr_tag::number){
    VM.return_value = Lisp_ptr{eql(*args[0].get<Number*>(), *args[1].get<Number*>())};
  }else{
    VM.return_value = Lisp_ptr{eq_internal(args[0], args[1])};
  }
}

constexpr struct Entry {
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
      whole_macro_and,
      Calling::whole_macro, {0, true}}},
  {"or", {
      whole_macro_or,
      Calling::whole_macro, {0, true}}},
  {"let", {
      whole_function_let,
      Calling::whole_function, {1, true}}},
  {"let*", {
      whole_function_let_star,
      Calling::whole_function, {1, true}}},
  {"letrec", {
      whole_function_letrec,
      Calling::whole_function, {1, true}}},
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
      procedure_list,
      Calling::function, {1, true}}},
  {"list*", {
      procedure_list_star,
      Calling::function, {1, true}}},
  {"vector", {
      procedure_vector, 
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

} //namespace

void install_builtin(){
  for(auto& e : builtin_func){
    VM.set(intern(VM.symtable, e.name), Lisp_ptr{&e.func});
  }
}
