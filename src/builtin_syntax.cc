#include <array>

#include "builtin_syntax.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "util.hh"
#include "procedure.hh"
#include "printer.hh"

using namespace std;
using namespace Procedure;

namespace {

void error_whole_function(const char* msg){
  auto wargs = pick_args_1();
  auto sym = wargs.get<Cons*>()->car().get<Symbol*>();

  assert(sym);

  fprintf(zs::err, "eval error: '%s' -- %s\n",
          sym->name().c_str(), msg);
  VM.return_value = {};
}

void whole_function_error(){
  error_whole_function("cannot be used as operator!!");
}

void whole_function_unimplemented(){
  error_whole_function("under development...");
}

void whole_function_pass_through(){
  VM.return_value = pick_args_1();
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
    VM.return_value = {};
    return;
  }
    
  VM.return_value = val;
}


static Lisp_ptr lambda_internal(Lisp_ptr args, Lisp_ptr code){
  auto arg_info = parse_func_arg(args);

  if(!arg_info){
    fprintf(zs::err, "eval error: lambda has invalid args!\n");
    return {};
  }
  if(!code){
    fprintf(zs::err, "eval error: lambda has invalid body!\n");
    return {};
  }
  
  return new IProcedure(code, Calling::function, arg_info, VM.frame);
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

  VM.return_value = lambda_internal(args, code);
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
    VM.return_value = {};
    return;
  }else if(len > 4){
    fprintf(zs::err, "eval error: informal if expr! (more than %d exprs)\n", len);
    VM.return_value = {};
    return;
  }

  // evaluating
  VM.code.push(vm_op_if);
  VM.code.push(test);

  VM.stack.push(alt);
  VM.stack.push(conseq);
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
    VM.return_value = {};
    return;
  }

  if(!val){
    fprintf(zs::err, "eval error: no value is supplied for %s\n", opname);
    VM.return_value = {};
    return;
  }

  if(len > 2){
    fprintf(zs::err, "eval error: informal %s expr! (more than %d exprs)\n", opname, len);
    VM.return_value = {};
    return;
  }

  // evaluating
  VM.code.push(set_op);
  VM.code.push(val);
  VM.stack.push(var);
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
      VM.return_value = {};
      return;
    }

    code = rest->cdr();

    auto value = lambda_internal(args, code);
    VM.local_set(var, value);
    VM.return_value = value;
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
    VM.return_value = {};
    return;
  }

  list_to_stack("begin", exprs, VM.code);
}

void whole_function_quasiquote(){
  auto wargs = pick_args_1();
  if(!wargs) return;

  bind_cons_list(wargs,
                 [](Cons*){},
                 [](Cons* c){
                   VM.code.push(c->car());
                 });
  VM.code.push(vm_op_quasiquote);
}

void whole_function_let(){
  let_internal(false, false);
}

void whole_function_let_star(){
  let_internal(true, true);
}

void whole_function_letrec(){
  let_internal(false, true);
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

constexpr BuiltinFunc
builtin_func[] = {
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
      Calling::whole_function, {0, false}}}
};

} //namespace

void install_builtin_syntax(){
  for(auto& e : builtin_func){
    VM.set(intern(VM.symtable, e.name), {&e.func});
  }
}