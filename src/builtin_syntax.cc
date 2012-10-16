#include <array>

#include "builtin_syntax.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "util.hh"
#include "procedure.hh"
#include "printer.hh"
#include "delay.hh"

using namespace std;
using namespace Procedure;

namespace {

void error_whole_function(const char* msg){
  auto wargs = pick_args_1();
  auto sym = wargs.get<Cons*>()->car().get<Symbol*>();

  assert(sym);

  fprintf(zs::err, "eval error: '%s' -- %s\n",
          sym->name().c_str(), msg);
  vm.return_value[0] = {};
}

void whole_function_error(){
  error_whole_function("cannot be used as operator!!");
}

void whole_function_unimplemented(){
  error_whole_function("under development...");
}

void whole_function_pass_through(){
  vm.return_value[0] = pick_args_1();
}

void whole_function_quote(){
  auto wargs = pick_args_1();
  if(!wargs) return;

  Lisp_ptr val;

  bind_cons_list(wargs,
                 [](Cons*){},
                 [&](Cons* c){
                   val = c->car();
                 },
                 [](Cons*){
                   fprintf(zs::err, "eval warning: quote has two or more args. ignored.\n");
                 });

  if(!val){
    fprintf(zs::err, "eval error: quote has no args.\n");
    vm.return_value[0] = {};
    return;
  }
    
  vm.return_value[0] = val;
}


static Lisp_ptr lambda_internal(Lisp_ptr args, Lisp_ptr code){
  auto arg_info = parse_func_arg(args);

  if(arg_info.first < 0){
    fprintf(zs::err, "eval error: lambda has invalid args!\n");
    return {};
  }
  if(!code){
    fprintf(zs::err, "eval error: lambda has invalid body!\n");
    return {};
  }
  
  return new IProcedure(code, 
                        {Calling::function, arg_info.first, arg_info.second},
                        args, vm.frame());
}

void whole_function_lambda(){
  auto wargs = pick_args_1();
  if(!wargs) return;

  Lisp_ptr args, code;

  bind_cons_list(wargs,
                 [](Cons*){},
                 [&](Cons* c){
                   args = c->car();
                   code = c->cdr();
                 });

  vm.return_value[0] = lambda_internal(args, code);
}

void whole_function_if(){
  auto wargs = pick_args_1();
  if(!wargs) return;

  // extracting
  Lisp_ptr test, conseq, alt;

  int len =
  bind_cons_list(wargs,
                 [](Cons*){},
                 [&](Cons* c){
                   test = c->car();
                 },
                 [&](Cons* c){
                   conseq = c->car();
                 },
                 [&](Cons* c){
                   alt = c->car();
                 });

  if(len < 3){
    fprintf(zs::err, "eval error: informal if expr! (only %d exprs)\n", len);
    vm.return_value[0] = {};
    return;
  }else if(len > 4){
    fprintf(zs::err, "eval error: informal if expr! (more than %d exprs)\n", len);
    vm.return_value[0] = {};
    return;
  }

  // evaluating
  vm.code.push_back(vm_op_if);
  vm.code.push_back(test);

  vm.stack.push_back(alt);
  vm.stack.push_back(conseq);
}

/*
  ----
  code = (value, VM::if)
  stack = (variable name)
*/
void set_internal(const char* opname, Lisp_ptr p, VMop set_op){
  // extracting
  Symbol* var = nullptr;
  Lisp_ptr val;

  int len =
    bind_cons_list(p,
                   [&](Cons* c){
                     var = c->car().get<Symbol*>();
                   },
                   [&](Cons* c){
                     val = c->car();
                   });

  if(!var){
    fprintf(zs::err, "eval error: variable's name is not a symbol!\n");
    vm.return_value[0] = {};
    return;
  }

  if(!val){
    fprintf(zs::err, "eval error: no value is supplied for %s\n", opname);
    vm.return_value[0] = {};
    return;
  }

  if(len > 2){
    fprintf(zs::err, "eval error: informal %s expr! (more than %d exprs)\n", opname, len);
    vm.return_value[0] = {};
    return;
  }

  // evaluating
  vm.code.push_back(set_op);
  vm.code.push_back(val);
  vm.stack.push_back(var);
}

void whole_function_set(){
  auto wargs = pick_args_1();
  if(!wargs) return;

  set_internal("set!", wargs.get<Cons*>()->cdr(), vm_op_set);
}

void whole_function_define(){
  auto wargs = pick_args_1();
  if(!wargs) return;

  auto p = wargs.get<Cons*>()->cdr();
  Cons* rest = p.get<Cons*>();

  // extracting
  auto first = rest->car();

  if(first.tag() == Ptr_tag::symbol){
    set_internal("define(value set)", p, vm_op_local_set);
  }else if(first.tag() == Ptr_tag::cons){
    Symbol* var = nullptr;
    Lisp_ptr args, code;

    bind_cons_list(first,
                   [&](Cons* c){
                     var = c->car().get<Symbol*>();
                     args = c->cdr();
                   });

    if(!var){
      fprintf(zs::err, "eval error: function's name is not a symbol!\n");
      vm.return_value[0] = {};
      return;
    }

    code = rest->cdr();

    auto value = lambda_internal(args, code);
    vm.local_set(var, value);
    vm.return_value[0] = value;
  }else{
    fprintf(zs::err, "eval error: informal define syntax!\n");
  }
}

void whole_function_begin(){
  auto wargs = pick_args_1();
  if(!wargs) return;

  auto exprs = wargs.get<Cons*>()->cdr();
  if(!exprs || nullp(exprs)){
    fprintf(zs::err, "eval error: begin has no exprs.\n");
    vm.return_value[0] = {};
    return;
  }

  list_to_stack("begin", exprs, vm.code);
}

void whole_function_quasiquote(){
  auto wargs = pick_args_1();
  if(!wargs) return;

  bind_cons_list(wargs,
                 [](Cons*){},
                 [](Cons* c){
                   vm.code.push_back(c->car());
                 });
  vm.code.push_back(vm_op_quasiquote);
}

void whole_function_let(){
  let_internal(Sequencial::f, EarlyBind::f);
}

void whole_function_let_star(){
  let_internal(Sequencial::t, EarlyBind::t);
}

void whole_function_letrec(){
  let_internal(Sequencial::f, EarlyBind::t);
}

Lisp_ptr whole_macro_or_expand(Cons* c){
  if(!c->cdr() || nullp(c->cdr())){
    return c->car();
  }

  const auto if_sym = intern(vm.symtable(), "if");
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

  const auto if_sym = intern(vm.symtable(), "if");
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
                             then_form = push_cons_list(intern(vm.symtable(), "begin"), c);
                           });
  assert(ret >= 1); // should be handled by previous tests.
  (void)ret;

  if(auto sym = test_form.get<Symbol*>()){
    if(sym->name() == "else"){
      return then_form;
    }
  }

  const auto if_sym = intern(vm.symtable(), "if");
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
    vm.return_value[0] = Lisp_ptr(default_value);
    return;
  }

  vm.return_value[0] = e(head);
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
  const auto eqv_sym = intern(vm.symtable(), "eqv?");
  auto eqv_expr = make_cons_list({eqv_sym, sym, keys->car()});

  if(!keys->cdr() || nullp(keys->cdr())){
    return eqv_expr;
  }

  const auto if_sym = intern(vm.symtable(), "if");
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
    vm.return_value[0] = Lisp_ptr();
    return;
  }

  // TODO: collect this by garbage collector!
  auto key_sym = new Symbol(new string("case_key_symbol"));

  vm.return_value[0] = 
    make_cons_list({intern(vm.symtable(), "let"),
          make_cons_list({
              make_cons_list({key_sym, key})
                }),
          new Cons(intern(vm.symtable(), "cond"),
                   whole_macro_case_expand(key_sym, clauses))
          });
}

void macro_delay(){
  auto args = pick_args_1();
  vm.return_value[0] = {new Delay(args, vm.frame())};
}

} //namespace

const BuiltinFunc
builtin_syntax[] = {
  {"quote", {
      whole_function_quote,
      {Calling::whole_function, 1}}},
  {"lambda", {
      whole_function_lambda,
      {Calling::whole_function, 1, Variadic::t}}},
  {"if", {
      whole_function_if,
      {Calling::whole_function, 3}}},
  {"set!", {
      whole_function_set,
      {Calling::whole_function, 2}}},
  {"define", {
      whole_function_define,
      {Calling::whole_function, 2, Variadic::t}}},
  {"quasiquote", {
      whole_function_quasiquote,
      {Calling::whole_function, 1}}},
  {"begin", {
      whole_function_begin,
      {Calling::whole_function, 1, Variadic::t}}},

  {"cond", {
      whole_macro_cond,
      {Calling::whole_macro, 1, Variadic::t}}},
  {"case", {
      whole_macro_case,
      {Calling::whole_macro, 2, Variadic::t}}},
  {"and", {
      whole_macro_and,
      {Calling::whole_macro, 0, Variadic::t}}},
  {"or", {
      whole_macro_or,
      {Calling::whole_macro, 0, Variadic::t}}},
  {"let", {
      whole_function_let,
      {Calling::whole_function, 1, Variadic::t}}},
  {"let*", {
      whole_function_let_star,
      {Calling::whole_function, 1, Variadic::t}}},
  {"letrec", {
      whole_function_letrec,
      {Calling::whole_function, 1, Variadic::t}}},
  {"do", {
      whole_function_unimplemented,
      {Calling::whole_function, 0, Variadic::t}}},
  {"delay", {
      macro_delay,
      {Calling::macro, 1}}},

  {"unquote", {
      whole_function_pass_through,
      {Calling::whole_function, 0, Variadic::t}}},
  {"unquote-splicing", {
      whole_function_pass_through,
      {Calling::whole_function, 0, Variadic::t}}},
  {"else", {
      whole_function_error,
      {Calling::whole_function, 0}}},
  {"=>", {
      whole_function_error,
      {Calling::whole_function, 0}}}
};

const size_t builtin_syntax_size = sizeof(builtin_syntax) / sizeof(builtin_syntax[0]);
