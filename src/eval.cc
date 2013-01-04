#include <cassert>
#include <iostream>
#include <functional>

#include "vm.hh"
#include "eval.hh"
#include "zs_error.hh"
#include "symbol.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "procedure.hh"
#include "printer.hh"
#include "builtin_util.hh"
#include "delay.hh"
#include "s_closure.hh"

using namespace std;
using namespace Procedure;

namespace {

static
void local_set_with_identifier(Lisp_ptr ident, Lisp_ptr value,
                               Env* default_env = vm.frame()){
  auto sym = identifier_symbol(ident);
  auto env = identifier_env(ident, default_env);

  env->local_set(sym, value);
}


void vm_op_proc_enter();

/*
  ret = some value
*/
void vm_op_arg_push(){
  assert(vm.code[vm.code.size() - 1].get<VMop>() == vm_op_arg_push);

  auto ret = vm.return_value[0];
  auto& argc = vm.code[vm.code.size() - 2];
  auto& args = vm.code[vm.code.size() - 3];

  if(nullp(args)){
    vm.stack.push_back(argc);
    vm.code.erase(vm.code.end() - 3, vm.code.end());
  }else{
    auto args_c = args.get<Cons*>();
    // auto arg1 = args_c->car(); // the EXPR just evaled.
    auto args_rest = args_c->cdr();
    
    vm.stack.push_back(ret);

    args = args_rest;
    argc = {Ptr_tag::vm_argcount, argc.get<int>() + 1};
    // vm.code[vm.code.size() - 1] = vm_op_begin;
    vm.code.push_back(args_rest.get<Cons*>()->car());
  }
}


/*
*/
void vm_op_call();

static void enter_frame(IProcedure* iproc){
  // tail call check
  if(!vm.code.empty()
     && vm.code.back().get<VMop>() == vm_op_leave_frame){
    vm_op_leave_frame();
  }

  auto oldenv = vm.frame();
  if(auto closure = iproc->closure()){
    vm.set_frame(closure->push());
  }else{
    vm.set_frame(vm.frame()->push());
  }
  vm.code.insert(vm.code.end(), {oldenv, vm_op_leave_frame});
}

void function_call(Lisp_ptr proc, const ProcInfo* info){
  auto args = vm.stack.back();
  vm.stack.pop_back();

  if(info->entering == Entering::at_bind){
    auto iproc = proc.get<IProcedure*>();
    assert(iproc);

    enter_frame(iproc);

    // captures arguments
    auto i = begin(iproc->arg_list());
    for(; i ; ++i){
      local_set_with_identifier(*i, Lisp_ptr{});
    }
    if(identifierp(i.base())){
      local_set_with_identifier(i.base(), Lisp_ptr{});
    }
  }

  auto args_head = args.get<Cons*>()->cdr(); // skips first symbol

  vm.code.insert(vm.code.end(), {proc, vm_op_proc_enter,
        args_head, {Ptr_tag::vm_argcount, 0},
        vm_op_arg_push, args_head.get<Cons*>()->car()});
}

/*
  ret = expanded proc
  ----
  code = proc
*/
void vm_op_macro_call(){
  assert(vm.code.back().get<VMop>() == vm_op_macro_call);
  vm.code.back() = vm.return_value[0];

  if(vm.return_value.size() > 1){
    vm.code.insert(vm.code.end(),
                   next(vm.return_value.begin()), vm.return_value.end());
    vm.return_value.resize(1);
  }
}  

/*
  return = an, an-1, ...
  code = (this), <VMop>, argcount[b], args(b)
  stack = bn, bn-1, ...
  ---
  return = {}
  code = (this), <VMop>, argcount[a+b], args(b)
  stack = bn, bn-1, ..., an, an-1, ...
*/
void vm_op_stack_splicing(){
  assert(vm.code.back() == vm_op_stack_splicing);

  auto& op = vm.code[vm.code.size() - 2];
  if(op.tag() != Ptr_tag::vm_op){
    throw zs_error("eval error: unquote-splicing: called in invalid context!\n");
  }

  auto& outer_argc = vm.code[vm.code.size() - 3];
  auto& outer_args = vm.code[vm.code.size() - 4];

  // pushes return-value to vm.stack
  int argc = vm.return_value.size();
  vm.stack.insert(vm.stack.end(), vm.return_value.begin(), vm.return_value.end());
  vm.return_value.resize(1);

  // formatting vm.code to 'splicing is processed'
  // see vm_op_arg_push()
  Lisp_ptr outer_next_args;
  bind_cons_list_loose
    (outer_args,
     [&](Lisp_ptr, ConsIter i){
      outer_next_args = i.base();
    });
  auto outer_next_arg1 = outer_next_args.get<Cons*>()->car();

  outer_argc = {Ptr_tag::vm_argcount, outer_argc.get<int>() + argc};
  outer_args = outer_next_args;
  vm.code.back() = outer_next_arg1;
}

/*
  stack[0] = whole args
  ----
  code = (call kind, proc, macro call)
  stack = (arg1, arg2, ..., arg-bottom)
*/
void macro_call(Lisp_ptr proc){
  auto args = vm.stack.back();
  vm.stack.pop_back();

  int argc = 0;
  for(auto p : args.get<Cons*>()->cdr()){
    vm.stack.push_back(p);
    ++argc;
  }
  vm.stack.push_back({Ptr_tag::vm_argcount, argc});

  proc_enter_entrypoint(proc); // direct jump to proc_enter()
}

/*
  stack[0] = whole args
  ----
  code = (call kind, proc)
  stack = (whole args, arg-bottom)
*/
void whole_call(Lisp_ptr proc, const ProcInfo* info){
  switch(info->required_args){
  case 1:
    vm.stack.push_back({Ptr_tag::vm_argcount, 1});
    break;
  case 2:
    vm.stack.push_back(vm.frame());
    vm.stack.push_back({Ptr_tag::vm_argcount, 2});
    break;
  default:
    throw zs_error("eval error: 'whole' function must take one or two args\n");
  }

  proc_enter_entrypoint(proc); // direct jump to proc_enter()
}

/*
  ret = proc
  stack[0] = args
  ---
  goto proc_call or macro_call
*/
void vm_op_call(){
  assert(vm.code.back().get<VMop>() == vm_op_call);

  auto proc = vm.return_value[0];

  if(!is_procedure(proc)){
    if(proc.tag() == Ptr_tag::cons){
      // special treating for an unevaluated lambda expr.
      auto first = proc.get<Cons*>()->car();
      if(identifierp(first)
         && identifier_symbol(first) == intern(vm.symtable(), "lambda")){
        vm.code.push_back(proc);
        return;
      }
    }

    vm.code.pop_back();
    vm.stack.pop_back();
    throw zs_error("eval error: (# # ...)'s first element is not procedure (got: %s)\n",
                   stringify(proc.tag()));
  }

  const ProcInfo* info = get_procinfo(proc);

  switch(info->returning){
  case Returning::pass:
    vm.code.pop_back();
    break;
  case Returning::code:
    vm.code.back() = vm_op_macro_call;
    break;
  case Returning::stack_splice:
    vm.code.back() = vm_op_stack_splicing;
    break;
  default:
    UNEXP_DEFAULT();
  }

  switch(info->passing){
  case Passing::eval:
    return function_call(proc, info);
  case Passing::quote:
    return macro_call(proc);
  case Passing::whole:
    return whole_call(proc, info);
  default:
    UNEXP_DEFAULT();
  }
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

  auto p = native_func();

  auto info = fun->info();
  assert(info);

  if(info->move_ret == MoveReturnValue::t){
    vm.return_value[0] = p;
  }
}

/*
  stack = (arg1, arg2, ..., arg-bottom)
  ----
  In new frame, args are bound.
  code = (body1, body2, ..., leave_frame)
  stack = ()
*/
void proc_enter_interpreted(IProcedure* fun, const ProcInfo* info){
  if(info->entering == Entering::at_jump){
    enter_frame(fun);
  }

  switch(info->leaving){
  case Leaving::immediate:
    break;
  case Leaving::after_returning_op: {
    // finds last leave_op
    decltype(vm.code.begin()) leave_op_i, b = vm.code.begin();
    for(leave_op_i = prev(vm.code.end()); leave_op_i > b; --leave_op_i){
      if(leave_op_i->tag() == Ptr_tag::vm_op 
         && leave_op_i->get<VMop>() == vm_op_leave_frame)
        break;
    }

    if(leave_op_i == b){
      throw zs_error("eval internal error: vm_op_leave_frame is not found!\n");
    }

    // replacing
    // {..., <VMop>, env, leave_frame} -> {..., env, leave_frame, <VMop>}
    auto env_i = prev(leave_op_i);
    if(env_i->tag() != Ptr_tag::env){
      throw zs_error("eval internal error: leave op has no env!\n");
    }

    if(env_i == b){
      throw zs_error("eval internal error: return op is not found!\n");
    }

    auto return_op_i = prev(env_i);
    if(return_op_i->tag() != Ptr_tag::vm_op){
      throw zs_error("eval internal error: return op is not VMop!\n");
    }

    std::swap(*return_op_i, *env_i);
    std::swap(*env_i, *leave_op_i);
    break;
  }
  default:
    UNEXP_DEFAULT();
  }

  // == processing args ==

  Lisp_ptr arg_name = fun->arg_list();
  Lisp_ptr argc = vm.stack.back();
  vm.stack.pop_back();

  auto arg_start = vm.stack.end() - argc.get<int>();
  auto arg_end = vm.stack.end();
  auto i = arg_start;

  // normal arg push
  for(; i != arg_end; ++i){
    auto arg_name_cell = arg_name.get<Cons*>();
    if(!arg_name_cell){
      break;
    }
   
    local_set_with_identifier(arg_name_cell->car(), *i);
    arg_name = arg_name_cell->cdr();
  }

  // variadic arg push
  if(info->max_args > info->required_args){
    if(!identifierp(arg_name)){
      throw zs_error("eval error: no arg name for variadic arg!\n");
    }

    auto var_argc = argc.get<int>() - static_cast<int>(std::distance(arg_start, i));
    vm.stack.erase(arg_start, i);
    vm.stack.push_back({Ptr_tag::vm_argcount, var_argc});
    local_set_with_identifier((arg_name), stack_to_list<false>(vm.stack));
  }else{  // clean stack
    if(i != arg_end){
      throw zs_error("eval error: corrupted stack -- passed too much args!\n");
    }
    vm.stack.erase(arg_start, arg_end);
  }

  // set up lambda body code
  vm.code.insert(vm.code.end(), {fun->get(), vm_op_begin});
}


static unsigned get_wind_index(const VM& va, const VM& vb){
  unsigned w = 0;
  while((w < va.extent.size())
        && (w < vb.extent.size())
        && (va.extent[w].thunk == vb.extent[w].thunk)){
    ++w;
  }
  return w;
}

void vm_op_restore_values(){
  assert(vm.code.back().get<VMop>() == vm_op_restore_values);
  vm.code.pop_back();

  auto values_p = vm.code.back();
  auto values = values_p.get<Vector*>();
  assert(values);
  vm.code.pop_back();

  vm.return_value = std::move(*values);
  delete values;
}

void vm_op_replace_vm(){
  assert(vm.code.back().get<VMop>() == vm_op_replace_vm);
  vm.code.pop_back();

  auto cont = vm.code.back();
  vm.code.pop_back();
  assert(cont.get<Continuation*>());

  auto values = vm.code.back();
  vm.code.pop_back();
  assert(values.tag() == Ptr_tag::vector);


  auto& next_vm = cont.get<Continuation*>()->get();
  
  // finds dynamic-winds for processing..
  const auto wind_index = get_wind_index(vm, next_vm);

  // ====== replaces vm! ======
  vm = next_vm;

  // restores old return values
  vm.code.insert(vm.code.end(), {values, vm_op_restore_values});

  // processes 'before' windings
  for(unsigned i = wind_index; i < vm.extent.size(); ++i){
    vm.stack.push_back({Ptr_tag::vm_argcount, 0});
    vm.code.insert(vm.code.end(), {vm.extent[i].before, vm_op_proc_enter});
  }
}

/*
  stack = (arg1, arg2, ..., arg-bottom)
  ----
  replaces VM!
*/
void proc_enter_cont(Continuation* c){
  auto& next_vm = c->get();

  // finds dynamic-winds for processing..
  const auto wind_index = get_wind_index(vm, next_vm);

  // saves old return values
  auto ret_values = new std::vector<Lisp_ptr>();
  stack_to_vector(vm.stack, *ret_values);

  vm.code.insert(vm.code.end(), {ret_values, c, vm_op_replace_vm});

  // processes 'after' windings
  for(unsigned i = vm.extent.size() - 1;
      (i >= wind_index) && (static_cast<signed>(i) >= 0);
      --i){
    vm.stack.push_back({Ptr_tag::vm_argcount, 0});
    vm.code.insert(vm.code.end(), {vm.extent[i].after, vm_op_proc_enter});
  }
}

} //namespace

void proc_enter_entrypoint(Lisp_ptr proc){
  assert(!vm.stack.empty());
  assert(is_procedure(proc));

  auto info = get_procinfo(proc);
  auto argc = vm.stack.back().get<int>();

  if(!(info->required_args <= argc && argc <= info->max_args)){
    // This local value is required, for avoiding a strange state.
    // If throws directly, std::uncaught_exception() returns true,
    // but std::current_exception() returns NULL object.
    auto e = builtin_argcount_failed("(unknown)", info->required_args,
                                     info->max_args, argc);
    throw e;
  }

  if(auto ifun = proc.get<IProcedure*>()){
    proc_enter_interpreted(ifun, info);
  }else if(auto nfun = proc.get<const NProcedure*>()){
    proc_enter_native(nfun);
  }else if(auto cont = proc.get<Continuation*>()){
    proc_enter_cont(cont);
  }else{
    throw zs_error("eval internal error: corrupted code stack -- no proc found for entering!\n");
  }
}

namespace {
 
/*
  code = (proc)
  ----
  code = ()
   and goto handler.
*/
void vm_op_proc_enter(){
  assert(vm.code.back().get<VMop>() == vm_op_proc_enter);
  vm.code.pop_back();

  auto proc = vm.code.back();
  vm.code.pop_back();

  proc_enter_entrypoint(proc);
}
 
/*
  ret = (args)
  ----
  stack = (args)
  goto proc_enter 
*/
void vm_op_move_values(){
  assert(vm.code.back().get<VMop>() == vm_op_move_values);

  vm.code.pop_back();
  int argc = vm.return_value.size();

  vm.stack.insert(vm.stack.end(), vm.return_value.begin(), vm.return_value.end());
  vm.stack.push_back({Ptr_tag::vm_argcount, argc});

  vm.return_value.resize(1);
}
 
} // namespace

/*
  leaves frame.
  no stack operations.
*/
void vm_op_leave_frame(){
  assert(vm.code.back().get<VMop>() == vm_op_leave_frame);
  vm.code.pop_back();

  auto oldenv = vm.code.back();
  assert(oldenv.tag() == Ptr_tag::env);
  vm.code.pop_back();

  vm.set_frame(oldenv.get<Env*>());
}  

/*
  code = [consequent, alternative]
  ----
  stack = (consequent or alternative)
*/
void vm_op_if(){
  assert(vm.code.back().get<VMop>() == vm_op_if);
  vm.code.pop_back();

  auto test_result = vm.return_value[0];

  if(test_result.get<bool>()){
    auto conseq = vm.code.back();
    vm.code.pop_back();
    vm.code.back() = conseq;
  }else{
    vm.code.pop_back();
  }
}

/*
  stack = (variable name)
  ----
  stack = ()
  return-value is setted.
*/
void vm_op_set(){
  assert(vm.code.back().tag() == Ptr_tag::vm_op);
  vm.code.pop_back();

  auto var = vm.code.back();
  vm.code.pop_back();

  if(!identifierp(var)){
    throw builtin_identifier_check_failed("(set)", vm.code.back());
  }
    
  auto sym = identifier_symbol(var);
  auto env = identifier_env(var, vm.frame());
  env->traverse(sym, vm.return_value[0]);
}

/*
  stack = (variable name)
  ----
  stack = ()
  return-value is setted.
*/
void vm_op_local_set(){
  assert(vm.code.back().tag() == Ptr_tag::vm_op);
  vm.code.pop_back();

  auto var = vm.code.back();
  vm.code.pop_back();

  if(!identifierp(var)){
    throw builtin_identifier_check_failed("(local set)", var);
  }

  local_set_with_identifier((var), vm.return_value[0]); 
}

/*
  code = [#1=(...)]
  ---
  (cdr #1) is not null.
    code = [(car #1) <vm-op-begin> (cdr #1)]
  (cdr #1) is null.
    code = [(car #1)]
 */
void vm_op_begin(){
  assert(vm.code[vm.code.size() - 1].get<VMop>() == vm_op_begin);

  auto& next = vm.code[vm.code.size() - 2];
  auto next_c = next.get<Cons*>();
  auto next_car = next_c->car();
  auto next_cdr = next_c->cdr();

  if(!nullp(next_cdr)){
    next = next_cdr;
    // vm.code[vm.code.size() - 1] = vm_op_begin;
    vm.code.push_back(next_car);
  }else{
    vm.code.pop_back();
    vm.code.back() = next_car;
  }
}

/*
  return  = forced value
  stack[0] = delay
*/
void vm_op_force(){
  assert(vm.code.back().get<VMop>() == vm_op_force);
  vm.code.pop_back();

  auto delay = vm.stack.back().get<Delay*>();
  vm.stack.pop_back();

  if(!delay){
    throw zs_error("eval error: internal error occured ('force' found before not a delay)\n");
  }

  delay->force(vm.return_value[0]);
}

Lisp_ptr let_internal(Entering entering){
  Lisp_ptr name = {};
  int len = 0;
  GrowList gl_syms;
  GrowList gl_vals;
  Lisp_ptr body;

  {
    ZsArgs wargs{1};

    // skips first 'let' symbol
    auto arg_c = wargs[0].get<Cons*>();
    assert(arg_c);

    auto arg = arg_c->cdr();
    if(arg.tag() != Ptr_tag::cons || nullp(arg)){
      throw zs_error("eval error: informal LET syntax -- (LET . <%s>).\n",
                          (nullp(arg)) ? "nil" : stringify(arg.tag()));
    }
    arg_c = arg.get<Cons*>();

    // checks named let
    if(arg_c->car().tag() == Ptr_tag::symbol){
      name = arg_c->car();

      arg = arg_c->cdr();
      if(arg.tag() != Ptr_tag::cons || nullp(arg)){
        throw zs_error("eval error: informal LET syntax -- (LET <name> . <%s>).\n",
                            (nullp(arg)) ? "nil" : stringify(arg.tag()));
      }
    
      arg_c = arg.get<Cons*>();
    }

    // picks elements
    auto binds = arg_c->car();
    body = arg_c->cdr();

    if(body.tag() != Ptr_tag::cons || nullp(body)){
      throw zs_error("eval error: informal syntax for LET's body!.\n");
    }

    // parses binding list
    for(auto bind : binds){
      if(bind.tag() != Ptr_tag::cons){
        throw zs_error("eval error: informal object (%s) found in let binding.\n",
                            stringify(bind.tag()));
      }

      auto c = bind.get<Cons*>();
      if(!c){
        throw zs_error("eval error: null found in let binding.\n");
      }
                 
      ++len;

      gl_syms.push(c->car());
      gl_vals.push(c->cdr().get<Cons*>()->car());
    }
  }

  if(name){
    auto oldenv = vm.frame();
    vm.set_frame(vm.frame()->push());
    vm.code.insert(vm.code.end(), {oldenv, vm_op_leave_frame});
  }

  auto proc = new IProcedure(body, 
                             {len, Variadic::f,  Passing::eval,
                                 Returning::pass, MoveReturnValue::t,
                                 entering},
                             gl_syms.extract(), vm.frame());

  if(name){
    local_set_with_identifier(name, proc);
  }

  vm.stack.push_back(push_cons_list({}, gl_vals.extract()));
  vm.code.insert(vm.code.end(), {vm_op_call, proc});
  return {};
}

void eval(){
  try{
    while(!vm.code.empty()){
      auto p = vm.code.back();

      switch(p.tag()){
      case Ptr_tag::symbol:
        vm.code.pop_back();
        vm.return_value[0] = vm.find(p.get<Symbol*>());
        break;
    
      case Ptr_tag::cons: {
        auto c = p.get<Cons*>();
        if(!c){
          vm.code.pop_back();
          vm.return_value[0] = Cons::NIL;
          break;
        }

        vm.code.back() = vm_op_call;
        vm.code.push_back(c->car());
        vm.stack.push_back(p);
        break;
      }

      case Ptr_tag::vm_op:
        if(auto op = p.get<VMop>()){
          op();
        }else{
          vm.code.pop_back();
        }
        break;

      case Ptr_tag::syntactic_closure: {
        auto sc = p.get<SyntacticClosure*>();
        assert(sc);

        auto oldenv = vm.frame();
        Env* newenv;

        if(!sc->free_names()){
          newenv = sc->env();
        }else{
          newenv = sc->env()->push();
          for(auto i : sc->free_names()){
            auto val = vm.find(identifier_symbol(i));
            local_set_with_identifier(i, val, newenv);
          }
        }

        vm.set_frame(newenv);
        vm.code.back() = oldenv;
        vm.code.push_back(vm_op_leave_frame);
        vm.code.push_back(sc->expr());
        break;
      }

        // self-evaluating
      case Ptr_tag::boolean: case Ptr_tag::character:
      case Ptr_tag::i_procedure: case Ptr_tag::n_procedure:
      case Ptr_tag::number:
      case Ptr_tag::string: case Ptr_tag::vector:
      case Ptr_tag::input_port: case Ptr_tag::output_port:
      case Ptr_tag::env:
      case Ptr_tag::delay:
      case Ptr_tag::continuation:
        vm.code.pop_back();
        vm.return_value[0] = p;
        break;

        // error
      case Ptr_tag::undefined:
        throw zs_error("eval error: undefined is passed!\n");

      case Ptr_tag::vm_argcount:
        throw zs_error("eval internal error: vm-argcount is rest on VM code stack!\n");

      default:
        throw zs_error("eval error: unknown object appeared! (tag = %d)!\n",
                            static_cast<int>(p.tag()));
      }
    }
  }catch(const std::exception& e){
    cerr << e.what() << endl;
    vm.return_value[0] = {};
  }

  if(!vm.code.empty()){
    cerr << "eval internal warning: VM code stack is broken!\n";
    cerr << "VM dump...\n";
    cerr << vm << endl;
    vm.code.clear();
  }

  if(!vm.stack.empty()){
    cerr << "eval internal warning: VM stack is broken! (stack has values unexpectedly.)\n";
    vm.stack.clear();
  }

  if(vm.return_value.empty()){
    vm.return_value.resize(1);
  }
}


Lisp_ptr func_force(){
  auto arg = pick_args_1();     // used because no exceptions are done!
  auto d = arg.get<Delay*>();
  if(!d){
    vm.return_value[0] = arg;
    return {};
  }
  
  if(d->forced()){
    vm.return_value[0] = d->get();
    return {};
  }

  auto oldenv = vm.frame();

  vm.set_frame(d->env());
  vm.stack.push_back(arg);
  vm.code.insert(vm.code.end(),
                 {vm_op_force, oldenv, vm_op_leave_frame, d->get()});
  return {};
}

Lisp_ptr call_with_values(){
  Lisp_ptr procs[2];
  {
    ZsArgs args{2};

    if(!is_procedure(args[0])){
      throw zs_error("call-with-values error: first arg is not procedure (%s)\n",
                          stringify(args[0].tag()));
    }

    auto info = get_procinfo(args[0]);
    if(info->required_args != 0){
      throw zs_error("call-with-values error: first arg takes 1 or more args (%d)\n",
                          info->required_args);
    }    

    if(!is_procedure(args[1])){
      throw zs_error("call-with-values error: second arg is not procedure (%s)\n",
                          stringify(args[1].tag()));
    }
    
    std::copy(args.begin(), args.end(), procs);
  }

  // second proc call
  vm.code.insert(vm.code.end(), {procs[1], vm_op_proc_enter, vm_op_move_values});

  // first proc, calling with zero args.
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  proc_enter_entrypoint(procs[0]); // direct jump to proc_enter()
  return {};
}

Lisp_ptr call_cc(){
  Lisp_ptr proc;
  {
    ZsArgs args{1};

    if(!is_procedure(args[0])){
      throw zs_error("call/cc error: first arg is not procedure (%s)\n",
                          stringify(args[0].tag()));
    }

    auto info = get_procinfo(args[0]);
    if(info->required_args != 1){
      throw zs_error("call/cc error: first arg mush take 1 arg (%d)\n",
                          info->required_args);
    }
    proc = args[0];
  }

  auto cont = new Continuation(vm);
  vm.stack.insert(vm.stack.end(), {cont, {Ptr_tag::vm_argcount, 1}});
  proc_enter_entrypoint(proc); // direct jump to proc_enter()
  return {};
}

/*
*/
static void vm_op_leave_winding(){
  assert(vm.code.back().get<VMop>() == vm_op_leave_winding);
  vm.code.pop_back();

  vm.extent.pop_back();
}

static void vm_op_save_values_and_enter(){
  assert(vm.code[vm.code.size() - 1].get<VMop>() == vm_op_save_values_and_enter);
  auto proc = vm.code[vm.code.size() - 2];

  vm.code[vm.code.size() - 2] = new Vector(vm.return_value);
  vm.code[vm.code.size() - 1] = vm_op_restore_values;
  
  proc_enter_entrypoint(proc);
}

Lisp_ptr dynamic_wind(){
  Lisp_ptr procs[3];
  {
    ZsArgs args{3};
    auto procs_i = begin(procs);

    for(auto p : args){
      if(!is_procedure(p)){
        throw zs_error("error: dynamic-wind: arg is not procedure (%s)\n",
                            stringify(p.tag()));
      }

      auto info = get_procinfo(p);
      if(info->required_args != 0){
        throw zs_error("error: dynamic-wind: first arg mush take 0 arg (%d)\n",
                            info->required_args);
      }

      *procs_i = p;
      ++procs_i;
    }
  }

  vm.extent.push_back({procs[0], procs[1], procs[2]});
  vm.code.push_back(vm_op_leave_winding);

  // third proc call
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  vm.code.push_back(procs[2]);
  vm.code.push_back(vm_op_save_values_and_enter);

  // second proc call
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  vm.code.push_back(procs[1]);
  vm.code.push_back(vm_op_proc_enter);

  // first proc, calling with zero args.
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  proc_enter_entrypoint(procs[0]); // direct jump to proc_enter()
  return {};
}

void vm_op_get_current_env(){
  assert(vm.code.back() == vm_op_get_current_env);
  vm.code.pop_back();
  
  vm.return_value[0] = vm.frame();
}

const char* stringify(VMop op){
  if(op == vm_op_nop){
    return "NOP / arg bottom";
  }else if(op == vm_op_proc_enter){
    return "proc enter";
  }else if(op == vm_op_arg_push){
    return "arg push";
  }else if(op == vm_op_macro_call){
    return "macro call";
  }else if(op == vm_op_call){
    return "call";
  }else if(op == vm_op_leave_frame){
    return "leave frame";
  }else if(op == vm_op_begin){
    return "begin";
  }else if(op == vm_op_restore_values){
    return "restore values";
  }else if(op == vm_op_replace_vm){
    return "replace vm";
  }else if(op == vm_op_proc_enter){
    return "proc enter";
  }else if(op == vm_op_move_values){
    return "move values";
  }else if(op == vm_op_if){
    return "if";
  }else if(op == vm_op_set){
    return "set";
  }else if(op == vm_op_local_set){
    return "local set";
  }else if(op == vm_op_force){
    return "force";
  }else if(op == vm_op_leave_winding){
    return "leave winding";
  }else if(op == vm_op_save_values_and_enter){
    return "save values and enter";
  }else if(op == vm_op_stack_splicing){
    return "splicing args";
  }else if(op == vm_op_get_current_env){
    return "get current env";
  }else{
    return "unknown vm-op";
  }
}
