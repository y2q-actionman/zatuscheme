#include <memory>
#include <cassert>

#include "eval.hh"
#include "util.hh"
#include "symbol.hh"
#include "cons.hh"
#include "procedure.hh"
#include "printer.hh"
#include "builtin.hh"
#include "builtin_util.hh"

using namespace std;
using namespace Procedure;

namespace {

Symbol* to_varname(Lisp_ptr p){
  Symbol* var = p.get<Symbol*>();

  if(!var){
    fprintf(stderr, "eval error: variable's name is not a symbol!\n");
    return nullptr;
  }

  return var;
}

void vm_op_proc_enter();

/*
  ret = some value
  ----
  stack[0] = some value
*/
void vm_op_arg_push(){
  VM.stack().push(VM.return_value());
}

/*
  stack[0] = whole args
  ----
  code = (argN, arg-move, argN-1, arg-move, ..., call kind, proc)
  stack = (arg-bottom)
*/
void function_call(Lisp_ptr proc, const ArgInfo* argi){
  auto args = VM.stack().top();
  VM.stack().pop();

  VM.code().push(proc);
  VM.code().push(Lisp_ptr(vm_op_proc_enter));

  int argc = 0;

  if(!do_list(args.get<Cons*>()->cdr(),
              [&](Cons* cell) -> bool{
                VM.code().push(Lisp_ptr(vm_op_arg_push));
                VM.code().push(cell->car());
                ++argc;

                return true;
              },
              [&](Lisp_ptr dot_cdr) -> bool{
                if(!nullp(dot_cdr)){
                  fprintf(stderr, "funcall error: arg-list is dot-list!\n");
                  return false;
                }
              
                if((argc < argi->required_args)
                   || (!argi->variadic && argc > argi->required_args)){
                  fprintf(stderr, "funcall error: number of passed args is mismatched!! (required %d args, %s, passed %d)\n",
                          argi->required_args,
                          (argi->variadic) ? "variadic" : "not variadic",
                          argc);
                  return false;
                }

                return true;
              })){
    for(int i = 0; i < argc*2 + 2; ++i){
      VM.code().pop();
    }
    VM.return_value() = {};
    return;
  }
  
  VM.stack().push(Lisp_ptr(vm_op_arg_bottom));
}

/*
  ret = expanded proc
  ----
  code = proc
*/
void vm_op_macro_call(){
  VM.code().push(VM.return_value());
}  

/*
  stack[0] = whole args
  ----
  code = (call kind, proc, macro call)
  stack = (arg1, arg2, ..., arg-bottom)
*/
void macro_call(Lisp_ptr proc, const ArgInfo* argi){
  auto args = VM.stack().top();
  VM.stack().pop();

  VM.stack().push(Lisp_ptr(vm_op_arg_bottom));
  auto argc = list_to_stack("macro-call", args.get<Cons*>()->cdr(), VM.stack());
  if(argc < argi->required_args
     || (!argi->variadic && argc > argi->required_args)){
    fprintf(stderr, "macro-call error: number of passed args is mismatched!! (required %d args, %s, passed %d)\n",
            argi->required_args,
            (argi->variadic) ? "variadic" : "not variadic",
            argc);
    for(int i = 0; i < argc; ++i){
      VM.stack().pop();
    }
    VM.stack().pop();
    VM.return_value() = {};
    return;
  }    

  VM.code().push(Lisp_ptr(vm_op_macro_call));
  VM.code().push(proc);
  VM.code().push(Lisp_ptr(vm_op_proc_enter));
}

/*
  stack[0] = whole args
  ----
  code = (call kind, proc)
  stack = (whole args, arg-bottom)
*/
void whole_function_call(Lisp_ptr proc){
  VM.code().push(proc);
  VM.code().push(Lisp_ptr(vm_op_proc_enter));

  auto args = VM.stack().top();
  VM.stack().pop();
  VM.stack().push(Lisp_ptr(vm_op_arg_bottom));
  VM.stack().push(args);
}

/*
  stack[0] = whole args
  ----
  code = (call kind, proc, macro call)
  stack = (whole args, arg-bottom)
*/
void whole_macro_call(Lisp_ptr proc){
  VM.code().push(Lisp_ptr(vm_op_macro_call));
  whole_function_call(proc);
}

/*
  ret = proc
  stack[0] = args
  ---
  goto proc_call or macro_call
*/
void vm_op_call(){
  auto proc = VM.return_value();

  Calling c;
  const ArgInfo* argi;

  if(auto ifun = proc.get<IProcedure*>()){
    c = ifun->calling();
    argi = &ifun->arg_info(); 
  }else if(auto nfun = proc.get<const NProcedure*>()){
    c = nfun->calling();
    argi = &nfun->arg_info();
  }else{
    fprintf(stderr, "eval error: (# # ...)'s first element is not procedure (got: %s)\n",
            stringify(proc.tag()));
    fprintf(stderr, "      expr: "); print(stderr, VM.stack().top()); fputc('\n', stderr);
    
    VM.return_value() = {};
    VM.stack().pop();
    return;
  }

  switch(c){
  case Calling::function:
    function_call(proc, argi); return;
  case Calling::macro:
    macro_call(proc, argi); return;
  case Calling::whole_function:
    whole_function_call(proc); return;
  case Calling::whole_macro:
    whole_macro_call(proc); return;
  default:
    UNEXP_DEFAULT();
  }
}
 
/*
  leaves frame.
  no stack operations.
*/
void vm_op_proc_leave(){
  VM.leave_frame();
}  

/*
  stack = (arg1, arg2, ..., arg-bottom)
  ----
  ret = returned value
  code = ()
  stack = ()
*/
void proc_enter_native(const NProcedure* fun){
  auto native_func = fun->get();
  assert(native_func);

  native_func();
  // if(!VM.return_value())
  //   fprintf(stderr, "eval warning: native func returned undef!\n");
}

/*
  stack = (arg1, arg2, ..., arg-bottom)
  ----
  In new frame, args are bound.
  code = (body1, body2, ..., leave_frame)
  stack = ()
*/
void proc_enter_interpreted(IProcedure* fun){
  const auto& argi = fun->arg_info();

  // tail call check
  if(!VM.code().empty()
     && VM.code().top().get<VMop>() == vm_op_proc_leave){
    VM.code().pop();
    VM.leave_frame();
  }

  VM.enter_frame(push_frame(fun->closure()));
  VM.code().push(Lisp_ptr(vm_op_proc_leave));

  Lisp_ptr arg_name = argi.head;
  Lisp_ptr st_top;

  // normal arg push
  while((st_top = VM.stack().top()).tag() != Ptr_tag::vm_op){
    VM.stack().pop();
    if(VM.stack().empty()){
      fprintf(stderr, "eval internal error: no args and no managed funcall!\n");
      VM.return_value() = {};
      return;
    }

    auto arg_name_cell = arg_name.get<Cons*>();
    if(!arg_name_cell){
      break;
    }
   
    VM.local_set(arg_name_cell->car().get<Symbol*>(), st_top);
    arg_name = arg_name_cell->cdr();
  }

  if(argi.variadic){   // variadic arg push
    if(!arg_name.get<Symbol*>()){
      fprintf(stderr, "eval error: no arg name for variadic arg!\n");
      VM.return_value() = {};
      return;
    }


    VM.local_set(arg_name.get<Symbol*>(), stack_to_list(VM.stack(), false));
  }else{  // clean stack
    if(VM.stack().empty()
       || VM.stack().top().tag() != Ptr_tag::vm_op){
      fprintf(stderr, "eval error: corrupted stack -- no bottom found!\n");
      VM.return_value() = {};
      return;
    }
    VM.stack().pop();
  }
  
  // set up lambda body code
  list_to_stack("funcall", fun->get(), VM.code());
}

/*
  code = (proc)
  ----
  code = ()
   and goto handler.
*/
void vm_op_proc_enter(){
  auto proc = VM.code().top();
  VM.code().pop();

  assert(!VM.stack().empty());

  if(auto ifun = proc.get<IProcedure*>()){
    proc_enter_interpreted(ifun);
  }else if(auto nfun = proc.get<const NProcedure*>()){
    proc_enter_native(nfun);
  }else{
    fprintf(stderr, "eval internal error: corrupted code stack -- no proc found in entering!\n");
    VM.return_value() = {};
    return;
  }
}
 
/*
  stack = (consequent, alternative)
  ----
  stack = ()
  code = (consequent or alternative)
*/
void vm_op_if(){
  auto test_result = VM.return_value();

  if(test_result.get<bool>()){
    VM.code().push(VM.stack().top());
    VM.stack().pop();
    VM.stack().pop();
  }else{
    VM.stack().pop();
    VM.code().push(VM.stack().top());
    VM.stack().pop();
  }
}

/*
  stack = (variable name)
  ----
  stack = ()
*/
void vm_op_set(){
  auto var = VM.stack().top().get<Symbol*>();
  VM.stack().pop();
  if(!var){
    fprintf(stderr, "eval error: internal error occured (set!'s varname is dismissed)\n");
    return;
  }

  VM.set(var, VM.return_value());
}

/*
  stack = (variable name)
  ----
  stack = ()
*/
void vm_op_local_set(){
  auto var = VM.stack().top().get<Symbol*>();
  VM.stack().pop();
  if(!var){
    fprintf(stderr, "eval error: internal error occured (set!'s varname is dismissed)\n");
    return;
  }

  VM.local_set(var, VM.return_value());
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
                     var = to_varname(c->car());
                   },
                   [&](Cons* c){
                     val = c->car();
                   });

  if(!var) return;

  if(!val){
    fprintf(stderr, "eval error: no value is supplied for %s\n", opname);
    VM.return_value() = {};
    return;
  }

  if(len > 2){
    fprintf(stderr, "eval error: informal %s expr! (more than %d exprs)\n", opname, len);
    VM.return_value() = {};
    return;
  }

  // evaluating
  VM.code().push(Lisp_ptr{set_op});
  VM.code().push(val);
  VM.stack().push(Lisp_ptr{var});
}

/*
  ret = list
  ----
  stack = (list[0], list[1], ...)
*/
void vm_op_arg_push_list(){
  list_to_stack("unquote-splicing", VM.return_value(), VM.stack());
}

static const VMop vm_op_quasiquote_list = procedure_list_star;
static const VMop vm_op_quasiquote_vector = procedure_vector;

/*
  code[0] = template
  ----
  * vector, list
      code = (quasiquote, template[0], arg_push, # normal
              template[1], arg_push,             # unquote
              template[2], arg_push_list,        # unquote-splicing
              ...,
              stack_to_{list or vector}
              )
      stack[0] = arg_bottom
  * default
      return = template
*/
void vm_op_quasiquote(){
  const auto unquote_sym = intern(VM.symtable, "unquote");
  const auto unquote_splicing_sym = intern(VM.symtable, "unquote-splicing");

  const auto qq_elem = [&](Lisp_ptr p){
    if(auto l = p.get<Cons*>()){
      if(auto l_first_sym = l->car().get<Symbol*>()){
        if(l_first_sym == unquote_sym){
          VM.code().push(Lisp_ptr(vm_op_arg_push));
          VM.code().push(l->cdr().get<Cons*>()->car());
          return;
        }else if(l_first_sym  == unquote_splicing_sym){
          VM.code().push(Lisp_ptr(vm_op_arg_push_list));
          VM.code().push(l->cdr().get<Cons*>()->car());
          return;
        }
      }
    }

    VM.code().push(Lisp_ptr(vm_op_arg_push));
    VM.code().push(p);
    VM.code().push(Lisp_ptr(vm_op_quasiquote));
  };

  auto p = VM.code().top();
  VM.code().pop();

  if(p.tag() == Ptr_tag::cons){
    if(nullp(p)){
      VM.return_value() = Cons::NIL;
      return;
    }

    // check unquote -- like `,x
    if(auto first_sym = p.get<Cons*>()->car().get<Symbol*>()){
      if(first_sym == unquote_sym){
        auto rest = p.get<Cons*>()->cdr().get<Cons*>()->car();
        VM.code().push(rest);
        return;
      }else if(first_sym == unquote_splicing_sym){
        fprintf(stderr, "eval error: unquote-splicing is not supported out of list");
        VM.return_value() = {};
        return;
      }
    }

    // generic lists
    VM.stack().push(Lisp_ptr(vm_op_arg_bottom));
    VM.code().push(Lisp_ptr(vm_op_quasiquote_list));

    do_list(p,
            [&](Cons* c) -> bool {
              qq_elem(c->car());
              return true;
            },
            [&](Lisp_ptr last){
              qq_elem(last);
            });
  }else if(p.tag() == Ptr_tag::vector){
    VM.stack().push(Lisp_ptr(vm_op_arg_bottom));
    VM.code().push(Lisp_ptr(vm_op_quasiquote_vector));

    auto v = p.get<Vector*>();
    for(auto i = begin(*v); i != end(*v); ++i){
      qq_elem(*i);
    }
  }else{
    VM.return_value() = p;
  }
}

Lisp_ptr pick_whole_arg(){
  return pick_args<1>()[0];
}

void error_whole_function(const char* msg){
  auto wargs = pick_whole_arg();
  auto sym = wargs.get<Cons*>()->car().get<Symbol*>();

  assert(sym);

  fprintf(stderr, "eval error: '%s' -- %s\n",
          sym->name().c_str(), msg);
  VM.return_value() = {};
}

} // namespace

void whole_function_error(){
  error_whole_function("cannot be used as operator!!");
}

void whole_function_unimplemented(){
  error_whole_function("under development...");
}

void whole_function_pass_through(){
  VM.return_value() = pick_whole_arg();
}

void whole_function_quote(){
  auto wargs = pick_whole_arg();
  if(!wargs) return;

  Lisp_ptr val;

  bind_cons_list(wargs,
                 [](Cons*){},
                 [&](Cons* c){
                   val = c->car();
                 },
                 [](Cons*){
                   fprintf(stderr, "eval warning: quote has two or more args. ignored.\n");
                 });

  if(!val){
    fprintf(stderr, "eval error: quote has no args.\n");
    VM.return_value() = {};
    return;
  }
    
  VM.return_value() = val;
}


static Lisp_ptr lambda_internal(Lisp_ptr args, Lisp_ptr code){
  auto arg_info = parse_func_arg(args);

  if(!arg_info){
    fprintf(stderr, "eval error: lambda has invalid args!\n");
    return {};
  }
  if(!code){
    fprintf(stderr, "eval error: lambda has invalid body!\n");
    return {};
  }
  
  return Lisp_ptr{new IProcedure(code, Calling::function, arg_info, VM.frame())};
}

void whole_function_lambda(){
  auto wargs = pick_whole_arg();
  if(!wargs) return;

  Lisp_ptr args, code;

  bind_cons_list(wargs,
                 [](Cons*){},
                 [&](Cons* c){
                   args = c->car();
                   code = c->cdr();
                 });

  VM.return_value() = lambda_internal(args, code);
}

void whole_function_if(){
  auto wargs = pick_whole_arg();
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
    fprintf(stderr, "eval error: informal if expr! (only %d exprs)\n", len);
    VM.return_value() = {};
    return;
  }else if(len > 4){
    fprintf(stderr, "eval error: informal if expr! (more than %d exprs)\n", len);
    VM.return_value() = {};
    return;
  }

  // evaluating
  VM.code().push(Lisp_ptr(vm_op_if));
  VM.code().push(test);

  VM.stack().push(alt);
  VM.stack().push(conseq);
}

void whole_function_set(){
  auto wargs = pick_whole_arg();
  if(!wargs) return;

  set_internal("set!", wargs.get<Cons*>()->cdr(), vm_op_set);
}

void whole_function_define(){
  auto wargs = pick_whole_arg();
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
                     var = to_varname(c->car());
                     args = (c->cdr());
                   });

    if(!var) return;

    code = rest->cdr();

    auto value = lambda_internal(args, code);
    VM.local_set(var, value);
    VM.return_value() = value;
  }else{
    fprintf(stderr, "eval error: informal define syntax!\n");
  }
}

void whole_function_begin(){
  auto wargs = pick_whole_arg();
  if(!wargs) return;

  auto exprs = wargs.get<Cons*>()->cdr();
  if(!exprs || nullp(exprs)){
    fprintf(stderr, "eval error: begin has no exprs.\n");
    VM.return_value() = {};
    return;
  }

  list_to_stack("begin", exprs, VM.code());
}

void whole_function_quasiquote(){
  auto wargs = pick_whole_arg();
  if(!wargs) return;

  bind_cons_list(wargs,
                 [](Cons*){},
                 [](Cons* c){
                   VM.code().push(c->car());
                 });
  VM.code().push(Lisp_ptr(vm_op_quasiquote));
}

void eval(){
  while(!VM.code().empty()){
    auto p = VM.code().top();
    VM.code().pop();

    switch(p.tag()){
    case Ptr_tag::undefined:
      fprintf(stderr, "eval error: undefined is passed!\n");
      VM.return_value() = {};
      break;

    case Ptr_tag::symbol:
      VM.return_value() = VM.find(p.get<Symbol*>());
      break;
    
    case Ptr_tag::cons: {
      auto c = p.get<Cons*>();
      if(!c){
        VM.return_value() = Cons::NIL;
        break;
      }

      VM.code().push(Lisp_ptr(vm_op_call));
      VM.code().push(c->car());
      VM.stack().push(p);
      break;
    }

    case Ptr_tag::vm_op:
      if(auto op = p.get<VMop>()){
        op();
      }
      break;

    case Ptr_tag::boolean: case Ptr_tag::character:
    case Ptr_tag::i_procedure: case Ptr_tag::n_procedure:
    case Ptr_tag::number:
    case Ptr_tag::string: case Ptr_tag::vector:
    case Ptr_tag::port: case Ptr_tag::env:
    default:
      VM.return_value() = p;
      break;
    }
  }

  if(!VM.stack().empty()){
    fprintf(stderr, "eval internal warning: VM stack is broken! (stack has values unexpectedly.)\n");
    do{
      VM.stack().pop();
    }while(!VM.stack().empty());
  }
}
