#include <memory>
#include <cassert>

#include "vm.hh"
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

void vm_op_proc_enter();

/*
  ret = some value
  ----
  stack[0] = some value
*/
void vm_op_arg_push(){
  VM.stack.push(VM.return_value);
}

/*
  ret = some value
  code[0] = symbol
  ----
  stack[0] = some value
  value is set to symbol
*/
void vm_op_arg_push_and_set(){
  auto symp = VM.code.top();
  VM.code.pop();

  assert(symp.tag() == Ptr_tag::symbol);
  VM.local_set(symp.get<Symbol*>(), VM.return_value);

  vm_op_arg_push();
}

/*
  stack[0] = whole args
  ----
  * normal call
    code = (argN, arg-move, argN-1, arg-move, ..., call kind, proc)
    stack = (arg-bottom)

  * sequencial call (let*)
    code = (argN, arg-set-move, symN, argN-1, arg-set-move, symN-1, ..., call kind, proc)
    stack = (arg-bottom)
*/
void function_call(Lisp_ptr proc, const ProcInfo* info, Lisp_ptr arg_head){
  auto args = VM.stack.top();
  VM.stack.pop();

  if(info->early_bind){
    auto iproc = proc.get<IProcedure*>();
    assert(iproc);
    VM.enter_frame(iproc->closure()->push());
  }

  VM.code.push(proc);
  VM.code.push(vm_op_proc_enter);

  int argc = 0;
  bool ret;

  if(!info->sequencial){
    ret = 
      do_list(args.get<Cons*>()->cdr(),
              [&](Cons* cell) -> bool{
                VM.code.push(vm_op_arg_push);
                VM.code.push(cell->car());
                ++argc;
                return true;
              },
              [&](Lisp_ptr dot_cdr) -> bool{
                if(!nullp(dot_cdr)){
                  fprintf(zs::err, "funcall error: argument binding failed.\n");
                  return false;
                }
                return true;
              });
  }else{
    Lisp_ptr bind_list = arg_head;

    ret = 
      do_list_2(args.get<Cons*>()->cdr(),
                bind_list,
                [&](Cons* cell, Cons* bindc) -> bool{
                  VM.code.push(bindc->car());
                  VM.code.push(vm_op_arg_push_and_set);
                  VM.code.push(cell->car());
                  ++argc;
                  return true;
                },
                [&](Lisp_ptr argc_cdr, Lisp_ptr bindc_cdr) -> bool{
                  if(!nullp(argc_cdr)){
                    if(nullp(bindc_cdr)){
                      fprintf(zs::err, "funcall internal error: sequencial calling cannot be variadic in this implementasion.\n");
                    }else{
                      fprintf(zs::err, "funcall error: argument binding failed.\n");
                    }
                    return false;
                  }
                  return true;
                });
  }
  
  if((argc < info->required_args)
     || (!info->variadic && argc > info->required_args)){
    fprintf(zs::err, "funcall error: number of passed args is mismatched!! (required %d args, %s, passed %d)\n",
            info->required_args,
            (info->variadic) ? "variadic" : "not variadic",
            argc);
    ret = false;
  }

  if(!ret){
    for(int i = 0; i < argc*2 + 2; ++i){
      VM.code.pop();
    }
    VM.return_value = {};
    return;
  }
  
  VM.stack.push(vm_op_arg_bottom);
}

/*
  ret = expanded proc
  ----
  code = proc
*/
void vm_op_macro_call(){
  VM.code.push(VM.return_value);
}  

/*
  stack[0] = whole args
  ----
  code = (call kind, proc, macro call)
  stack = (arg1, arg2, ..., arg-bottom)
*/
void macro_call(Lisp_ptr proc, const ProcInfo* info){
  auto args = VM.stack.top();
  VM.stack.pop();

  VM.stack.push(vm_op_arg_bottom);
  auto argc = list_to_stack("macro-call", args.get<Cons*>()->cdr(), VM.stack);
  if(argc < info->required_args
     || (!info->variadic && argc > info->required_args)){
    fprintf(zs::err, "macro-call error: number of passed args is mismatched!! (required %d args, %s, passed %d)\n",
            info->required_args,
            (info->variadic) ? "variadic" : "not variadic",
            argc);
    for(int i = 0; i < argc; ++i){
      VM.stack.pop();
    }
    VM.stack.pop();
    VM.return_value = {};
    return;
  }    

  VM.code.push(vm_op_macro_call);
  VM.code.push(proc);
  VM.code.push(vm_op_proc_enter);
}

/*
  stack[0] = whole args
  ----
  code = (call kind, proc)
  stack = (whole args, arg-bottom)
*/
void whole_function_call(Lisp_ptr proc){
  VM.code.push(proc);
  VM.code.push(vm_op_proc_enter);

  auto args = VM.stack.top();
  VM.stack.pop();
  VM.stack.push(vm_op_arg_bottom);
  VM.stack.push(args);
}

/*
  stack[0] = whole args
  ----
  code = (call kind, proc, macro call)
  stack = (whole args, arg-bottom)
*/
void whole_macro_call(Lisp_ptr proc){
  VM.code.push(vm_op_macro_call);
  whole_function_call(proc);
}

/*
  ret = proc
  stack[0] = args
  ---
  goto proc_call or macro_call
*/
void vm_op_call(){
  auto proc = VM.return_value;

  const ProcInfo* info;
  Lisp_ptr args;

  if(auto ifun = proc.get<IProcedure*>()){
    info = ifun->info(); 
    args = ifun->arg_head();
  }else if(auto nfun = proc.get<const NProcedure*>()){
    info = nfun->info();
    args = {};
  }else{
    fprintf(zs::err, "eval error: (# # ...)'s first element is not procedure (got: %s)\n",
            stringify(proc.tag()));
    fprintf(zs::err, "      expr: "); print(zs::err, VM.stack.top()); fputc('\n', zs::err);
    
    VM.return_value = {};
    VM.stack.pop();
    return;
  }

  switch(info->calling){
  case Calling::function:
    function_call(proc, info, args); return;
  case Calling::macro:
    macro_call(proc, info); return;
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
  //   fprintf(zs::err, "eval warning: native func returned undef!\n");
}

/*
  stack = (arg1, arg2, ..., arg-bottom)
  ----
  In new frame, args are bound.
  code = (body1, body2, ..., leave_frame)
  stack = ()
*/
void proc_enter_interpreted(IProcedure* fun){
  const auto& argi = fun->info();

  // tail call check
  if(!VM.code.empty()
     && VM.code.top().get<VMop>() == vm_op_proc_leave){
    VM.code.pop();
    VM.leave_frame();
  }

  if(!fun->info()->early_bind){
    VM.enter_frame(fun->closure()->push());
  }

  VM.code.push(vm_op_proc_leave);

  Lisp_ptr arg_name = fun->arg_head();
  Lisp_ptr st_top;

  // normal arg push
  while((st_top = VM.stack.top()).tag() != Ptr_tag::vm_op){
    VM.stack.pop();
    if(VM.stack.empty()){
      fprintf(zs::err, "eval internal error: no args and no managed funcall!\n");
      VM.return_value = {};
      return;
    }

    auto arg_name_cell = arg_name.get<Cons*>();
    if(!arg_name_cell){
      break;
    }
   
    VM.local_set(arg_name_cell->car().get<Symbol*>(), st_top);
    arg_name = arg_name_cell->cdr();
  }

  if(argi->variadic){   // variadic arg push
    if(!arg_name.get<Symbol*>()){
      fprintf(zs::err, "eval error: no arg name for variadic arg!\n");
      VM.return_value = {};
      return;
    }

    // TODO: share code below with procedure_list()
    VM.local_set(arg_name.get<Symbol*>(), stack_to_list<false>(VM.stack));
  }else{  // clean stack
    if(VM.stack.empty()
       || VM.stack.top().tag() != Ptr_tag::vm_op){
      fprintf(zs::err, "eval error: corrupted stack -- no bottom found!\n");
      VM.return_value = {};
      return;
    }
    VM.stack.pop();
  }
  
  // set up lambda body code
  list_to_stack("funcall", fun->get(), VM.code);
}

/*
  code = (proc)
  ----
  code = ()
   and goto handler.
*/
void vm_op_proc_enter(){
  auto proc = VM.code.top();
  VM.code.pop();

  assert(!VM.stack.empty());

  if(auto ifun = proc.get<IProcedure*>()){
    proc_enter_interpreted(ifun);
  }else if(auto nfun = proc.get<const NProcedure*>()){
    proc_enter_native(nfun);
  }else{
    fprintf(zs::err, "eval internal error: corrupted code stack -- no proc found in entering!\n");
    VM.return_value = {};
    return;
  }
}
 
/*
  ret = list
  ----
  stack = (list[0], list[1], ...)
*/
void vm_op_arg_push_list(){
  list_to_stack("unquote-splicing", VM.return_value, VM.stack);
}

static const VMop vm_op_quasiquote_list = procedure_list_star;
static const VMop vm_op_quasiquote_vector = procedure_vector;

} // namespace

/*
  stack = (consequent, alternative)
  ----
  stack = ()
  code = (consequent or alternative)
*/
void vm_op_if(){
  auto test_result = VM.return_value;

  if(test_result.get<bool>()){
    VM.code.push(VM.stack.top());
    VM.stack.pop();
    VM.stack.pop();
  }else{
    VM.stack.pop();
    VM.code.push(VM.stack.top());
    VM.stack.pop();
  }
}

/*
  stack = (variable name)
  ----
  stack = ()
  return-value is setted.
*/
void vm_op_set(){
  auto var = VM.stack.top().get<Symbol*>();
  VM.stack.pop();
  if(!var){
    fprintf(zs::err, "eval error: internal error occured (set!'s varname is dismissed)\n");
    return;
  }

  VM.set(var, VM.return_value);
}

/*
  stack = (variable name)
  ----
  stack = ()
  return-value is setted.
*/
void vm_op_local_set(){
  auto var = VM.stack.top().get<Symbol*>();
  VM.stack.pop();
  if(!var){
    fprintf(zs::err, "eval error: internal error occured (set!'s varname is dismissed)\n");
    return;
  }

  VM.local_set(var, VM.return_value);
}

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
          VM.code.push(vm_op_arg_push);
          VM.code.push(l->cdr().get<Cons*>()->car());
          return;
        }else if(l_first_sym  == unquote_splicing_sym){
          VM.code.push(vm_op_arg_push_list);
          VM.code.push(l->cdr().get<Cons*>()->car());
          return;
        }
      }
    }

    VM.code.push(vm_op_arg_push);
    VM.code.push(p);
    VM.code.push(vm_op_quasiquote);
  };

  auto p = VM.code.top();
  VM.code.pop();

  if(p.tag() == Ptr_tag::cons){
    if(nullp(p)){
      VM.return_value = Cons::NIL;
      return;
    }

    // check unquote -- like `,x
    if(auto first_sym = p.get<Cons*>()->car().get<Symbol*>()){
      if(first_sym == unquote_sym){
        auto rest = p.get<Cons*>()->cdr().get<Cons*>()->car();
        VM.code.push(rest);
        return;
      }else if(first_sym == unquote_splicing_sym){
        fprintf(zs::err, "eval error: unquote-splicing is not supported out of list");
        VM.return_value = {};
        return;
      }
    }

    // generic lists
    VM.stack.push(vm_op_arg_bottom);
    VM.code.push(vm_op_quasiquote_list);

    do_list(p,
            [&](Cons* c) -> bool {
              qq_elem(c->car());
              return true;
            },
            [&](Lisp_ptr last){
              qq_elem(last);
            });
  }else if(p.tag() == Ptr_tag::vector){
    VM.stack.push(vm_op_arg_bottom);
    VM.code.push(vm_op_quasiquote_vector);

    auto v = p.get<Vector*>();
    for(auto i = begin(*v); i != end(*v); ++i){
      qq_elem(*i);
    }
  }else{
    VM.return_value = p;
  }
}

void let_internal(Sequencial sequencial, EarlyBind early_bind){
  auto arg = pick_args_1();
  if(!arg) return;

  // skips first 'let' symbol
  auto arg_c = arg.get<Cons*>();
  assert(arg_c);

  auto arg2 = arg_c->cdr();
  if(arg2.tag() != Ptr_tag::cons || nullp(arg2)){
    fprintf(zs::err, "eval error: informal LET syntax -- (LET . <%s>).\n",
            (nullp(arg2)) ? "nil" : stringify(arg2.tag()));
    VM.return_value = {};
    return;
  }
  arg_c = arg2.get<Cons*>();

  // checks named let
  Lisp_ptr name = {};

  if(arg_c->car().tag() == Ptr_tag::symbol){
    name = arg_c->car();

    auto arg3 = arg_c->cdr();
    if(arg3.tag() != Ptr_tag::cons || nullp(arg3)){
      fprintf(zs::err, "eval error: informal LET syntax -- (LET <name> . <%s>).\n",
              (nullp(arg3)) ? "nil" : stringify(arg3.tag()));
      VM.return_value = {};
      return;
    }
    
    arg_c = arg3.get<Cons*>();
  }
    
  // picks elements
  auto binds = arg_c->car();
  auto body = arg_c->cdr();

  if(body.tag() != Ptr_tag::cons || nullp(body)){
    fprintf(zs::err, "eval error: informal syntax for LET's body!.\n");
    VM.return_value = {};
    return;
  }

  // parses binding list
  int len = 0;
  Lisp_ptr syms = Cons::NIL, vals = Cons::NIL;

  if(!do_list(binds,
              [&](Cons* cell) -> bool{
                auto bind = cell->car();
                if(bind.tag() != Ptr_tag::cons){
                  fprintf(zs::err, "eval error: informal object (%s) found in let binding.\n",
                          stringify(bind.tag()));
                  return false;
                }

                auto c = bind.get<Cons*>();
                if(!c){
                  fprintf(zs::err, "eval error: null found in let binding.\n");
                  return false;
                }
                 
                ++len;
                syms = push_cons_list(c->car(), syms);
                vals = push_cons_list(c->cdr().get<Cons*>()->car(), vals);
                return true;
              },
              [&](Lisp_ptr dot_cdr){
                return nullp(dot_cdr);
              })){
    fprintf(zs::err, "eval error: let binding was failed!\n");
    free_cons_list(syms);
    free_cons_list(vals);
    VM.return_value = {};
    return;
  }

  if(name){
    VM.enter_frame(VM.frame->push());
  }

  auto proc = new IProcedure(body, 
                             {Calling::function, len, Variadic::f, sequencial, early_bind},
                             syms, VM.frame);

  if(name){
    VM.local_set(name.get<Symbol*>(), proc);
    VM.code.push(vm_op_proc_leave);
  }

  VM.code.push(vm_op_call);
  VM.code.push(proc);
  VM.stack.push(push_cons_list({}, vals));
  VM.return_value = {};
}

void eval(){
  while(!VM.code.empty()){
    auto p = VM.code.top();
    VM.code.pop();

    switch(p.tag()){
    case Ptr_tag::undefined:
      fprintf(zs::err, "eval error: undefined is passed!\n");
      VM.return_value = {};
      break;

    case Ptr_tag::symbol:
      VM.return_value = VM.find(p.get<Symbol*>());
      break;
    
    case Ptr_tag::cons: {
      auto c = p.get<Cons*>();
      if(!c){
        VM.return_value = Cons::NIL;
        break;
      }

      VM.code.push(vm_op_call);
      VM.code.push(c->car());
      VM.stack.push(p);
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
      VM.return_value = p;
      break;
    }
  }

  if(!VM.stack.empty()){
    fprintf(zs::err, "eval internal warning: VM stack is broken! (stack has values unexpectedly.)\n");
    do{
      VM.stack.pop();
    }while(!VM.stack.empty());
  }
}
