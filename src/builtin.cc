#include <array>

#include "builtin.hh"
#include "util.hh"
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
    fprintf(zs::err, "native func '+': first arg is not number! %s\n",
            stringify(args[0].tag()));
    return;
  }

  Number* n2 = args[1].get<Number*>();
  if(!n2){
    fprintf(zs::err, "native func '+': second arg is not number! %s\n",
            stringify(args[1].tag()));
    return;
  }

  Number* newn = new Number(n1->get<long>() + n2->get<long>());
  VM.return_value = newn;
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
                             || (arg.tag() == Ptr_tag::n_procedure)};
}

Lisp_ptr whole_macro_or_expand(Cons* c){
  if(!c->cdr() || nullp(c->cdr())){
    return c->car();
  }

  const auto if_sym = intern(VM.symtable, "if");
  auto else_clause = whole_macro_or_expand(c->cdr().get<Cons*>());

  return make_cons_list({if_sym,
        c->car(),
        vm_op_nop,
        else_clause});
}

Lisp_ptr whole_macro_and_expand(Cons* c){
  if(!c->cdr() || nullp(c->cdr())){
    return c->car();
  }

  const auto if_sym = intern(VM.symtable, "if");
  auto then_clause = whole_macro_and_expand(c->cdr().get<Cons*>());

  return make_cons_list({if_sym,
        c->car(),
        then_clause,
        vm_op_nop});
}

Lisp_ptr whole_macro_cond_expand(Cons* head){
  if(!head) return {}; // unspecified in R5RS.

  auto clause = head->car();
  if(!clause.get<Cons*>()){
    fprintf(zs::err, "macro cond: informal clause syntax! '");
    print(zs::err, head->car());
    fprintf(zs::err, "'\n");
    return {};
  }

  Lisp_ptr test_form;
  Lisp_ptr then_form;

  int ret = bind_cons_list(clause,
                           [&](Cons* c){
                             test_form = c->car();
                             if(nullp(c->cdr())){
                               then_form = vm_op_nop;
                             }
                           },
                           [&](Cons* c){
                             if(auto sym = c->car().get<Symbol*>()){
                               if(sym->name() == "=>"){
                                 fprintf(zs::err, "macto cond: sorry, cond's => syntax is not implemented..\n");
                                 then_form = {};
                                 return;
                               }
                             }
                             then_form = push_cons_list(intern(VM.symtable, "begin"), c);
                           });
  assert(ret >= 1); // should be handled by previous tests.
  (void)ret;

  if(auto sym = test_form.get<Symbol*>()){
    if(sym->name() == "else"){
      return then_form;
    }
  }

  const auto if_sym = intern(VM.symtable, "if");
  auto else_form = whole_macro_cond_expand(head->cdr().get<Cons*>());

  return make_cons_list({if_sym,
        test_form,
        then_form,
        else_form});
}

template<typename T, typename Expander>
inline
void whole_macro_conditional(T default_value, Expander e){
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
  whole_macro_conditional(true, whole_macro_and_expand);
}
                 
void whole_macro_or(){
  whole_macro_conditional(false, whole_macro_or_expand);
}

void whole_macro_cond(){
  whole_macro_conditional(Lisp_ptr{}, whole_macro_cond_expand);
}

Lisp_ptr whole_macro_case_keys_expand(Symbol* sym, Cons* keys){
  const auto eqv_sym = intern(VM.symtable, "eqv?");
  auto eqv_expr = make_cons_list({eqv_sym, sym, keys->car()});

  if(!keys->cdr() || nullp(keys->cdr())){
    return eqv_expr;
  }

  const auto if_sym = intern(VM.symtable, "if");
  auto else_clause = whole_macro_case_keys_expand(sym, keys->cdr().get<Cons*>());

  return make_cons_list({if_sym,
        eqv_expr,
        Lisp_ptr(true),
        else_clause});
}

Lisp_ptr whole_macro_case_expand(Symbol* sym, Lisp_ptr cases_ptr){
  if(cases_ptr.tag() != Ptr_tag::cons){
    fprintf(zs::err, "macro case: informal case syntax (dot-list?): ");
    print(zs::err, cases_ptr);
    fputc('\n', zs::err);
    return {};
  }

  auto cases = cases_ptr.get<Cons*>();
  if(!cases)
    return Cons::NIL;

  auto clause = cases->car().get<Cons*>();
  if(!clause){
    fprintf(zs::err, "macro case: informal clause contained: ");
    print(zs::err, cases->car());
    fputc('\n', zs::err);
    return {};
  }

  auto keys_ptr = clause->car();
  Symbol* keys_sym;
  Lisp_ptr new_keys;

  if(auto keys_lst = keys_ptr.get<Cons*>()){
    new_keys = whole_macro_case_keys_expand(sym, keys_lst);
  }else if((keys_sym = keys_ptr.get<Symbol*>())
           && keys_sym->name() == "else"){
    new_keys = keys_ptr;
  }else{
    fprintf(zs::err, "macro case: informal clause key: ");
    print(zs::err, keys_ptr);
    fputc('\n', zs::err);
    return {};
  }

  auto new_clause = new Cons(new_keys, clause->cdr());

  return {new Cons(new_clause,
                   whole_macro_case_expand(sym, cases->cdr()))};
}

void whole_macro_case(){
  auto arg = pick_args_1();
  if(!arg) return;

  Lisp_ptr key, clauses;

  int len = bind_cons_list(arg,
                           [](Cons*){},
                           [&](Cons* c){
                             key = c->car();
                             clauses = c->cdr();
                           });
  if(len < 3){
    fprintf(zs::err, "macro case: invalid syntax! (no key found)\n");
    VM.return_value = Lisp_ptr();
    return;
  }

  // TODO: collect this by garbage collector!
  auto key_sym = new Symbol(new string("case_key_symbol"));

  VM.return_value = 
    make_cons_list({intern(VM.symtable, "let"),
          make_cons_list({
              make_cons_list({key_sym, key})
                }),
          new Cons(intern(VM.symtable, "cond"),
                   whole_macro_case_expand(key_sym, clauses))
          });
}
  


bool eq_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() != b.tag()) return false;

  if(a.tag() == Ptr_tag::boolean){
    return a.get<bool>() == b.get<bool>();
  }else if(a.tag() == Ptr_tag::character){
     // this can be moved into eqv? in R5RS, but char is contained in Lisp_ptr.
    return a.get<char>() == b.get<char>();
  }else{
    return a.get<void*>() == b.get<void*>();
  }
}

bool eqv_internal(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() == Ptr_tag::number && b.tag() == Ptr_tag::number){
    return eqv(*a.get<Number*>(), *b.get<Number*>());
  }else{
    return eq_internal(a, b);
  }
}

void eq(){
  auto args = pick_args<2>();
  VM.return_value = Lisp_ptr{eq_internal(args[0], args[1])};
}

void eqv(){
  auto args = pick_args<2>();
  VM.return_value = Lisp_ptr{eqv_internal(args[0], args[1])};
}

void eval_func(){
  auto args = pick_args<2>();
  
  // TODO: uses arg2 as Env struct.
  VM.code.push(args[0]);
}

void to_macro_procedure(){
  auto arg1 = pick_args_1();

  if(arg1.tag() != Ptr_tag::i_procedure){
    fprintf(zs::err, "to-macro-procedure: error: should be called with interpreted proc\n");
    VM.return_value = {};
    return;
  }

  auto proc = arg1.get<IProcedure*>();

  VM.return_value = new IProcedure(proc->get(), Calling::macro,
                                   proc->arg_info(), proc->closure());
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
      whole_macro_cond,
      Calling::whole_macro, {1, true}}},
  {"case", {
      whole_macro_case,
      Calling::whole_macro, {2, true}}},
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
  {"eqv?", {
      eqv,
      Calling::function, {2, false}}},
  {"eq?", {
      eq,
      Calling::function, {2, false}}},

  {"eval", {
      eval_func,
      Calling::function, {2, false}}},
  {"to-macro-procedure", {
      to_macro_procedure,
      Calling::function, {1, false}}}
};

} //namespace

void install_builtin(){
  for(auto& e : builtin_func){
    VM.set(intern(VM.symtable, e.name), {&e.func});
  }
}
