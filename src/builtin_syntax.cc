#include <array>
#include <iostream>

#include "builtin_syntax.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "util.hh"
#include "procedure.hh"
#include "printer.hh"
#include "delay.hh"
#include "cons_util.hh"

using namespace std;
using namespace Procedure;

namespace {

void error_whole_function(const char* msg){
  auto wargs = pick_args_1();
  auto sym = wargs.get<Cons*>()->car().get<Symbol*>();

  assert(sym);

  throw make_zs_error("eval error: '%s' -- %s\n",
                      sym->name().c_str(), msg);
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
                   throw zs_error("eval error: quote has two or more args.\n");
                 });
  if(!val){
    throw zs_error("eval error: quote has no args.\n");
  }
    
  vm.return_value[0] = val;
}


static Lisp_ptr lambda_internal(Lisp_ptr args, Lisp_ptr code){
  auto arg_info = parse_func_arg(args);

  if(arg_info.first < 0){
    throw zs_error("eval error: lambda has invalid args!\n");
  }
  if(!code){
    throw zs_error("eval error: lambda has invalid body!\n");
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
    throw make_zs_error("eval error: informal if expr! (only %d exprs)\n", len);
  }else if(len > 4){
    throw make_zs_error("eval error: informal if expr! (more than %d exprs)\n", len);
  }

  // evaluating
  vm.code.insert(vm.code.end(), {vm_op_if, test});

  vm.stack.insert(vm.stack.end(), {alt, conseq});
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
    throw zs_error("eval error: variable's name is not a symbol!\n");
  }

  if(!val){
    throw make_zs_error("eval error: no value is supplied for %s\n", opname);
  }

  if(len > 2){
    throw make_zs_error("eval error: informal %s expr! (more than %d exprs)\n", opname, len);
  }

  // evaluating
  vm.code.insert(vm.code.end(), {set_op, val});
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
      throw zs_error("eval error: function's name is not a symbol!\n");
    }

    code = rest->cdr();

    auto value = lambda_internal(args, code);
    vm.local_set(var, value);
    vm.return_value[0] = value;
  }else{
    throw zs_error("eval error: informal define syntax!\n");
  }
}

void whole_function_begin(){
  auto wargs = pick_args_1();
  if(!wargs) return;

  auto exprs = wargs.get<Cons*>()->cdr();
  if(!exprs || nullp(exprs)){
    throw zs_error("eval error: begin has no exprs.\n");
  }

  // list_to_stack("begin", exprs, vm.code);
  vm.code.insert(vm.code.end(), {exprs, vm_op_begin});
}

void whole_function_let(){
  let_internal(EarlyBind::f);
}

Lisp_ptr let_star_expand(Lisp_ptr bindings, Lisp_ptr body){
  if(nullp(bindings)){
    const auto begin_sym = intern(vm.symtable(), "begin");
    return new Cons(begin_sym, body);
  }else{
    const auto let_sym = intern(vm.symtable(), "let");

    Lisp_ptr b_first, b_rest;
    bind_cons_list(bindings,
                   [&](Cons* c){
                     b_first = c->car();
                     b_rest = c->cdr();
                   });

    return make_cons_list({let_sym,
                           make_cons_list({b_first}),
                           let_star_expand(b_rest, body)});
  }
}

void whole_function_let_star(){
  auto wargs = pick_args_1();
  if(!wargs) return;

  Lisp_ptr bindings, body;

  int len = bind_cons_list(wargs,
                           [](Cons*){}, // let* symbol
                           [&](Cons* c){
                             bindings = c->car();
                             body = c->cdr();
                           });
  if(len < 2){
    vm.return_value[0] = {};
    return;
  }

  vm.stack.insert(vm.stack.end(),
                  {let_star_expand(bindings, body), {Ptr_tag::vm_argcount, 1}});

  let_internal(EarlyBind::t);
}

void whole_function_letrec(){
  let_internal(EarlyBind::t);
}

Lisp_ptr or_expand(Cons* c){
  if(!c->cdr() || nullp(c->cdr())){
    return c->car();
  }

  const auto if_sym = intern(vm.symtable(), "if");
  auto else_clause = or_expand(c->cdr().get<Cons*>());

  return make_cons_list({if_sym,
        c->car(),
        vm_op_nop,
        else_clause});
}

Lisp_ptr and_expand(Cons* c){
  if(!c->cdr() || nullp(c->cdr())){
    return c->car();
  }

  const auto if_sym = intern(vm.symtable(), "if");
  auto then_clause = and_expand(c->cdr().get<Cons*>());

  return make_cons_list({if_sym,
        c->car(),
        then_clause,
        vm_op_nop});
}

Lisp_ptr cond_expand(Cons* head){
  if(!head) return {}; // unspecified in R5RS.

  auto clause = head->car();
  if(!clause.get<Cons*>()){
    throw zs_error("macro cond: informal clause syntax! \n");
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
                                 throw zs_error("macto cond: sorry, cond's => syntax is not implemented..\n");
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
  auto else_form = cond_expand(head->cdr().get<Cons*>());

  return make_cons_list({if_sym,
        test_form,
        then_form,
        else_form});
}

template<typename T, typename Expander>
inline
void whole_conditional(T default_value, Expander e){
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

  vm.code.push_back(e(head));
}

void whole_and(){
  whole_conditional(true, and_expand);
}
                 
void whole_or(){
  whole_conditional(false, or_expand);
}

void whole_cond(){
  whole_conditional(Lisp_ptr{}, cond_expand);
}

Lisp_ptr case_keys_expand(Symbol* sym, Cons* keys){
  const auto eqv_sym = intern(vm.symtable(), "eqv?");
  auto eqv_expr = make_cons_list({eqv_sym, sym, keys->car()});

  if(!keys->cdr() || nullp(keys->cdr())){
    return eqv_expr;
  }

  const auto if_sym = intern(vm.symtable(), "if");
  auto else_clause = case_keys_expand(sym, keys->cdr().get<Cons*>());

  return make_cons_list({if_sym,
        eqv_expr,
        Lisp_ptr(true),
        else_clause});
}

Lisp_ptr case_expand(Symbol* sym, Lisp_ptr cases_ptr){
  if(cases_ptr.tag() != Ptr_tag::cons){
    throw zs_error("macro case: informal case syntax (dot-list?)\n");
  }

  auto cases = cases_ptr.get<Cons*>();
  if(!cases)
    return Cons::NIL;

  auto clause = cases->car().get<Cons*>();
  if(!clause){
    throw zs_error("macro case: informal clause contained\n");
  }

  auto keys_ptr = clause->car();
  Lisp_ptr new_keys;

  if(auto keys_lst = keys_ptr.get<Cons*>()){
    new_keys = case_keys_expand(sym, keys_lst);
  }else if(auto keys_sym = keys_ptr.get<Symbol*>()){
    if(keys_sym->name() != "else"){
      throw make_zs_error("macro case: informal clause key symbol: %s\n", keys_sym->name().c_str());
    }else{
      new_keys = keys_ptr;
    }
  }else{
    throw zs_error("macro case: informal clause key\n");
  }

  return push_cons_list(push_cons_list(new_keys, clause->cdr()),
                        case_expand(sym, cases->cdr()));
}

void whole_case(){
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
    throw zs_error("macro case: invalid syntax! (no key found)\n");
  }

  // TODO: collect this by garbage collector!
  auto key_sym = new Symbol(new string("case_key_symbol"));

  auto form = 
    make_cons_list({intern(vm.symtable(), "let"),
          make_cons_list({
              make_cons_list({key_sym, key})
                }),
          new Cons(intern(vm.symtable(), "cond"),
                   case_expand(key_sym, clauses))
          });
  vm.code.push_back(form);
}

void whole_do(){
  auto arg = pick_args_1();
  if(!arg) return;

  Lisp_ptr vars, end_test, end_exprs,  commands;

  int len = bind_cons_list(arg,
                           [](Cons*){}, // 'do' symbol
                           [&](Cons* c){
                             vars = c->car();
                           },
                           [&](Cons* c){
                             auto ends = c->car();
                             auto end_c = ends.get<Cons*>();
                             end_test = end_c->car();
                             end_exprs = end_c->cdr();
                           },
                           [&](Cons* c){
                             commands = c;
                           });
  if(len < 3){
    throw make_zs_error("macro do: invalid syntax! (length is %d, less than 3)\n", len);
  }

  // TODO: collect this by garbage collector!
  auto loop_sym = new Symbol(new string("do_loop_symbol"));

  // extract vars
  GrowList init_binds;
  GrowList steps;
  do_list(vars,
          [&](Cons* vc) -> bool{
            Lisp_ptr v, i, s;
            bind_cons_list(vc->car(),
                           [&](Cons* c){ v = c->car(); },
                           [&](Cons* c){ i = c->car(); },
                           [&](Cons* c){ s = c->car(); });
            if(!s) s = v;

            init_binds.push(make_cons_list({v, i}));
            steps.push(s);
            return true;
          },
          [&](Lisp_ptr p){
            if(!nullp(p)){
              throw zs_error("macro do error: loop vars has dot-list!\n");
            }
          });

  // creates loop body
  GrowList gw;

  gw.push(intern(vm.symtable(), "begin"));
  do_list(commands,
          [&](Cons* c) -> bool{
            gw.push(c->car());
            return true;
          },
          [&](Lisp_ptr p){
            if(!nullp(p)){
              throw zs_error("macro do error: loop body has dot-list!\n");
            }
          });
  gw.push(push_cons_list(loop_sym, steps.extract()));

  // creates 'named let' style loop
  auto form = 
    make_cons_list({
        intern(vm.symtable(), "let"),
        loop_sym,
        init_binds.extract(),
        make_cons_list({
            intern(vm.symtable(), "if"),
            end_test,
            push_cons_list(intern(vm.symtable(), "begin"), end_exprs),
            gw.extract(),
            })
        });
  vm.code.push_back(form);
}

void macro_delay(){
  auto args = pick_args_1();
  vm.return_value[0] = {new Delay(args, vm.frame())};
}

/*
  stack = argcount[a], an, an-1, ..., argcount[b], bn, bn-1, ...
  ---
  stack = argcount[a+b], an, an-1, ..., bn, bn-1, ...
*/
void function_splicing(){
  auto args = pick_args_1();

  if(vm.code.empty()
     || vm.code.back().tag() != Ptr_tag::vm_op){
    throw zs_error("eval error: unquote-splicing: called in invalid context!\n");
  }

  // auto& op = vm.code[vm.code.size() - 1];
  auto& parent_argc = vm.code[vm.code.size() - 2];
  auto& parent_args = vm.code[vm.code.size() - 3];

  // pushes unquote-splicing's return-value to vm.stack
  int argc = 0;
  do_list(args,
          [&](Cons* c) -> bool {
            vm.stack.push_back(c->car());
            ++argc;
            return true;
          },
          [&](Lisp_ptr last_cdr){
            if(!nullp(last_cdr)){
              cerr << "eval warning: unquote-splicing: ";
              if(argc > 0){
                cerr << "dot list has read as proper list.\n";
              }else{
                cerr << "passed value is not list. treated as a list has one element.\n";
              }
              vm.stack.push_back(last_cdr);
              ++argc;
            }
          });

  // formatting vm.code to 'unquote-splicing is processed'
  // see vm_op_arg_push()
  Lisp_ptr parent_next_args, parent_next_arg1;
  bind_cons_list(parent_args,
                 [](Cons*){}, // (unquote-splicing ...)
                 [&](Cons* c){
                   parent_next_args = c;
                   parent_next_arg1 = c->car();
                 });

  parent_argc = {Ptr_tag::vm_argcount, parent_argc.get<int>() + argc};
  parent_args = parent_next_args;
  vm.code.push_back(parent_next_arg1);
}

void whole_function_quasiquote(){
  Lisp_ptr arg;
  bind_cons_list(pick_args_1(),
                 [](Cons*){}, // skips 'quasiquote' symbol
                 [&](Cons* c){
                   arg = c->car();
                 });

  if(arg.tag() != Ptr_tag::cons && arg.tag() != Ptr_tag::vector){
    // acting as a normal quote.
    vm.return_value[0] = arg;
    return;
  }


  const auto quasiquote_sym = intern(vm.symtable(), "quasiquote");
  const auto unquote_sym = intern(vm.symtable(), "unquote");
  const auto unquote_splicing_sym = intern(vm.symtable(), "unquote-splicing");

  GrowList gl;

  const auto qq_elem = [&](Lisp_ptr p){
    if(auto l = p.get<Cons*>()){
      if(auto l_first_sym = l->car().get<Symbol*>()){
        if(l_first_sym == unquote_sym){
          gl.push(l->cdr().get<Cons*>()->car());
          return;
        }else if(l_first_sym  == unquote_splicing_sym){
          gl.push(l);
          return;
        }
      }
    }

    gl.push(make_cons_list({quasiquote_sym, p}));
  };

  if(arg.tag() == Ptr_tag::cons){
    if(nullp(arg)){
      vm.return_value[0] = Cons::NIL;
      return;
    }

    // check unquote -- like `,x
    if(auto first_sym = arg.get<Cons*>()->car().get<Symbol*>()){
      if(first_sym == unquote_sym){
        vm.code.push_back(arg.get<Cons*>()->cdr().get<Cons*>()->car());
        return;
      }else if(first_sym == unquote_splicing_sym){
        throw zs_error("quasiquote error: unquote-splicing is not supported out of list");
      }
    }

    // generic lists
    do_list(arg,
            [&](Cons* cell) -> bool {
              qq_elem(cell->car());
              return true;
            },
            [&](Lisp_ptr last){
              qq_elem(last);
            });

    vm.code.push_back(push_cons_list(intern(vm.symtable(), "list*"), gl.extract()));
  }else if(arg.tag() == Ptr_tag::vector){
    auto v = arg.get<Vector*>();
    for(auto i = begin(*v); i != end(*v); ++i){
      qq_elem(*i);
    }

    vm.code.push_back(push_cons_list(intern(vm.symtable(), "vector"), gl.extract()));
  }else{
    UNEXP_DEFAULT();
  }
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
  {"begin", {
      whole_function_begin,
      {Calling::whole_function, 1, Variadic::t}}},

  {"cond", {
      whole_cond,
      {Calling::whole_function, 1, Variadic::t}}},
  {"case", {
      whole_case,
      {Calling::whole_function, 2, Variadic::t}}},
  {"and", {
      whole_and,
      {Calling::whole_function, 0, Variadic::t}}},
  {"or", {
      whole_or,
      {Calling::whole_function, 0, Variadic::t}}},
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
      whole_do,
      {Calling::whole_function, 2, Variadic::t}}},
  {"delay", {
      macro_delay,
      {Calling::macro, 1}}},

  {"quasiquote", {
      whole_function_quasiquote,
      {Calling::whole_function, 1}}},
  {"unquote", {
      whole_function_pass_through,
      {Calling::whole_function, 1}}},
  {"unquote-splicing", {
      function_splicing,
      {Calling::function, 1}}},

  {"else", {
      whole_function_error,
      {Calling::whole_function, 0}}},
  {"=>", {
      whole_function_error,
      {Calling::whole_function, 0}}}
};

const size_t builtin_syntax_size = sizeof(builtin_syntax) / sizeof(builtin_syntax[0]);
