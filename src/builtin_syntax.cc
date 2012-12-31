#include <string>
#include <iostream>

#include "builtin_syntax.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "zs_error.hh"
#include "procedure.hh"
#include "printer.hh"
#include "delay.hh"
#include "cons_util.hh"

using namespace std;
using namespace Procedure;

Lisp_ptr whole_function_error(){
  ZsArgs wargs{1};
  auto sym = wargs[0].get<Cons*>()->car().get<Symbol*>();
  assert(sym);

  throw zs_error("eval error: '%s' -- cannot be used as operator!!\n",
                      sym->name().c_str());
}

Lisp_ptr whole_function_pass_through(){
  ZsArgs wargs{1};
  return wargs[0];
}

Lisp_ptr syntax_quote(){
  ZsArgs args{1};
  return args[0];
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
                        {arg_info.first, arg_info.second},
                        args, vm.frame());
}

Lisp_ptr syntax_lambda(){
  ZsArgs wargs{1};

  return bind_cons_list_strict
    (wargs[0],
     [](Symbol*, Lisp_ptr args, ConsIter code){
      return lambda_internal(args, code.base());
    });
}

Lisp_ptr syntax_if(){
  ZsArgs args;

  assert(args.size() == 2 || args.size() == 3);

  Lisp_ptr test = args[0];
  Lisp_ptr conseq = args[1];
  Lisp_ptr alt = (args.size() == 3) ? args[2] : Lisp_ptr();
    
  vm.return_value = {alt, conseq, vm_op_if, test};
  return {};
}

Lisp_ptr syntax_set(){
  ZsArgs args{2};

  Symbol* var = args[0].get<Symbol*>();
  if(!var){
    throw builtin_type_check_failed("set!", Ptr_tag::symbol, args[0]);
  }

  vm.return_value = {var, vm_op_set, args[1]};
  return {};
}

Lisp_ptr syntax_define(){
  ZsArgs args{1};

  auto p = args[0].get<Cons*>()->cdr();
  Cons* rest = p.get<Cons*>();

  // extracting
  auto first = rest->car();

  if(first.tag() == Ptr_tag::symbol){
    return bind_cons_list_strict
      (p,
       [&](Symbol* var, Lisp_ptr expr) -> Lisp_ptr {
        vm.code.insert(vm.code.end(), {var, vm_op_local_set, expr});
        return expr;
      });
  }else if(first.tag() == Ptr_tag::cons){
    Lisp_ptr code = rest->cdr();

    return
      bind_cons_list_strict
      (first,
       [&](Symbol* var, ConsIter l_args) -> Lisp_ptr{
        auto value = lambda_internal(l_args.base(), code);
        vm.local_set(var, value);
        return value;
      });
  }else{
    throw zs_error("eval error: informal define syntax!\n");
  }
}

Lisp_ptr syntax_begin(){
  ZsArgs wargs{1};

  return bind_cons_list_strict
    (wargs[0],
     [](Symbol*, ConsIter body) -> Lisp_ptr{
      if(!body){
        throw zs_error("eval error: begin has no exprs.\n");
      }

      vm.return_value = {body.base(), vm_op_begin};
      return {};
    });
}

Lisp_ptr syntax_let(){
  return let_internal(EarlyBind::f);
}

static
Lisp_ptr let_star_expand(Lisp_ptr bindings, Lisp_ptr body){
  if(nullp(bindings)){
    const auto begin_sym = intern(vm.symtable(), "begin");
    return new Cons(begin_sym, body);
  }else{
    const auto let_sym = intern(vm.symtable(), "let");

    return bind_cons_list_strict
      (bindings,
       [&](Lisp_ptr b_first, ConsIter b_rest) -> Lisp_ptr {
        return make_cons_list({let_sym,
                               make_cons_list({b_first}),
                               let_star_expand(b_rest.base(), body)});
      });
  }
}

Lisp_ptr syntax_let_star(){
  Lisp_ptr bindings, body;

  {
    ZsArgs wargs{1};

    bind_cons_list_strict
      (wargs[0],
       [&](Symbol*, Lisp_ptr bindings1, ConsIter body1){
        bindings = bindings1;
        body = body1.base();
      });
  }

  vm.stack.insert(vm.stack.end(),
                  {let_star_expand(bindings, body), {Ptr_tag::vm_argcount, 1}});

  return let_internal(EarlyBind::f);
}

Lisp_ptr syntax_letrec(){
  // This heavyly depends on the implementation.
  //
  // normal let / funtion call
  //   1. evaluate args
  //      when a lambda form occurs, captures outer environment.
  //   2. enter a new environment.
  //   3. set args to the environment.
  // 
  // early bind
  //   1. enter a new environment.
  //   2. evaluate args
  //      when a lambda form occurs, captures the environment.
  //      so these lambdas refer the same environment.
  //   3. set args to the environment.
  return let_internal(EarlyBind::t);
}

static
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

static
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

static
Lisp_ptr cond_expand(Cons* head){
  if(!head) return {}; // unspecified in R5RS.

  auto clause = head->car();
  if(!clause.get<Cons*>()){
    throw zs_error("macro cond: informal clause syntax! \n");
  }

  Lisp_ptr test_form;
  Lisp_ptr then_form;

  bind_cons_list_strict
    (clause,
     [&](Lisp_ptr test, ConsIter last_i) -> void{
      test_form = test;
      
      auto last = last_i.base();

      if(nullp(last)){
        then_form = vm_op_nop;
        return;
      }
      
      if(auto sym = last.get<Symbol*>()){
        if(sym->name() == "=>"){
          throw zs_error("macro cond: sorry, cond's => syntax is not implemented..\n");
          return;
        }
      }
      
      then_form = push_cons_list(intern(vm.symtable(), "begin"), last);
    });

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
static inline
Lisp_ptr syntax_conditional(T default_value, Expander e){
  ZsArgs wargs{1};

  Cons* head;

  bind_cons_list_loose
    (wargs[0],
     [&](Symbol*, ConsIter rest){
      head = rest.base().get<Cons*>();
    });

  if(!head){
    return Lisp_ptr(default_value);
  }

  return e(head);
}

Lisp_ptr syntax_and(){
  return syntax_conditional(true, and_expand);
}
                 
Lisp_ptr syntax_or(){
  return syntax_conditional(false, or_expand);
}

Lisp_ptr syntax_cond(){
  return syntax_conditional(Lisp_ptr{}, cond_expand);
}

static
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

static
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
      throw zs_error("macro case: informal clause key symbol: %s\n", keys_sym->name().c_str());
    }else{
      new_keys = keys_ptr;
    }
  }else{
    throw zs_error("macro case: informal clause key\n");
  }

  return push_cons_list(push_cons_list(new_keys, clause->cdr()),
                        case_expand(sym, cases->cdr()));
}

Lisp_ptr syntax_case(){
  ZsArgs wargs{1};

  Lisp_ptr key, clauses;

  bind_cons_list_strict
    (wargs[0],
     [&](Symbol*, Lisp_ptr key1, ConsIter clauses1) -> void{
      key = key1;
      clauses = clauses1.base();
    });

  // TODO: collect this by garbage collector!
  auto key_sym = new Symbol(new string("case_key_symbol"));

  return
    make_cons_list({intern(vm.symtable(), "let"),
          make_cons_list({
              make_cons_list({key_sym, key})
                }),
          new Cons(intern(vm.symtable(), "cond"),
                   case_expand(key_sym, clauses))
          });
}

Lisp_ptr syntax_do(){
  ZsArgs wargs{1};

  Lisp_ptr vars, end_test, end_exprs,  commands;

  bind_cons_list_strict
    (wargs[0],
     [&](Symbol*, Lisp_ptr vars1, Lisp_ptr ends, ConsIter commands1) -> void{
      vars = vars1;

      auto end_c = ends.get<Cons*>();
      end_test = end_c->car();
      end_exprs = end_c->cdr();

      commands = commands1.base();
    });

  // TODO: collect this by garbage collector!
  auto loop_sym = new Symbol(new string("do_loop_symbol"));

  // extract vars
  GrowList init_binds;
  GrowList steps;

  for(auto p : vars){
    bind_cons_list_loose
      (p,
       [&](Lisp_ptr v, Lisp_ptr i, Lisp_ptr s) -> void{
        if(!s) s = v;

        init_binds.push(make_cons_list({v, i}));
        steps.push(s);
      });
  }

  // creates loop body
  GrowList gw;

  gw.push(intern(vm.symtable(), "begin"));
  for(auto p : commands){
    gw.push(p);
  }
  gw.push(push_cons_list(loop_sym, steps.extract()));

  // creates 'named let' style loop
  return
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
}

Lisp_ptr macro_delay(){
  ZsArgs wargs{1};
  return {new Delay(wargs[0], vm.frame())};
}

/*
  stack = argcount[a], an, an-1, ..., argcount[b], bn, bn-1, ...
  ---
  stack = argcount[a+b], an, an-1, ..., bn, bn-1, ...
*/
static
Lisp_ptr function_splicing(){
  if(vm.code.empty()
     || vm.code.back().tag() != Ptr_tag::vm_op){
    throw zs_error("eval error: unquote-splicing: called in invalid context!\n");
  }

  auto args = pick_args_1();

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
  Lisp_ptr parent_next_args;
  bind_cons_list_loose
    (parent_args,
     [&](Lisp_ptr, ConsIter i){
      parent_next_args = i.base();
    });
  auto parent_next_arg1 = parent_next_args.get<Cons*>()->car();

  parent_argc = {Ptr_tag::vm_argcount, parent_argc.get<int>() + argc};
  parent_args = parent_next_args;
  vm.code.push_back(parent_next_arg1);
  return vm_op_nop;
}

Lisp_ptr whole_function_quasiquote(){
  ZsArgs wargs{1};

  Lisp_ptr arg;

  bind_cons_list_strict
    (wargs[0],
     [&](Symbol*, Lisp_ptr expr){
      arg = expr;
    });

  if(arg.tag() != Ptr_tag::cons && arg.tag() != Ptr_tag::vector){
    // acting as a normal quote.
    return arg;
  }


  const auto quasiquote_sym = intern(vm.symtable(), "quasiquote");
  const auto unquote_sym = intern(vm.symtable(), "unquote");
  const auto unquote_splicing_sym = intern(vm.symtable(), "unquote-splicing");

  static const Procedure::NProcedure splicing_nproc{
    function_splicing, {1}
  };

  GrowList gl;

  const auto qq_elem = [&](Lisp_ptr p){
    if(auto l = p.get<Cons*>()){
      if(auto l_first_sym = l->car().get<Symbol*>()){
        if(l_first_sym == unquote_sym){
          gl.push(l->cdr().get<Cons*>()->car());
          return;
        }else if(l_first_sym == unquote_splicing_sym){
          gl.push(push_cons_list(&splicing_nproc, l->cdr()));
          return;
        }
      }
    }

    gl.push(make_cons_list({quasiquote_sym, p}));
  };

  if(arg.tag() == Ptr_tag::cons){
    if(nullp(arg)){
      return Cons::NIL;
    }

    // check unquote -- like `,x
    if(auto first_sym = arg.get<Cons*>()->car().get<Symbol*>()){
      if(first_sym == unquote_sym){
        vm.code.push_back(arg.get<Cons*>()->cdr().get<Cons*>()->car());
        return vm_op_nop;
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
    return vm_op_nop;
  }else if(arg.tag() == Ptr_tag::vector){
    auto v = arg.get<Vector*>();
    for(auto i = begin(*v); i != end(*v); ++i){
      qq_elem(*i);
    }

    vm.code.push_back(push_cons_list(intern(vm.symtable(), "vector"), gl.extract()));
    return vm_op_nop;
  }else{
    UNEXP_DEFAULT();
  }
}
