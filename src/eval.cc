#include <cassert>
#include <iostream>

#include "vm.hh"
#include "eval.hh"
#include "util.hh"
#include "symbol.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "procedure.hh"
#include "printer.hh"
#include "builtin_util.hh"
#include "delay.hh"

using namespace std;
using namespace Procedure;

namespace {

void vm_op_proc_enter();
void proc_enter_entrypoint(Lisp_ptr);


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

void function_call(Lisp_ptr proc, const ProcInfo* info){
  assert(vm.code.back().get<VMop>() == vm_op_call);

  auto args = vm.stack.back();
  vm.stack.pop_back();

  if(info->early_bind){
    auto iproc = proc.get<IProcedure*>();
    assert(iproc);

    if(auto closure = iproc->closure()){
      vm.enter_frame(closure->push());
    }else{
      vm.enter_frame(vm.frame()->push());
    }
  }

  auto args_head = args.get<Cons*>()->cdr(); // skips first symbol

  vm.code.back() = proc;
  vm.code.insert(vm.code.end(), {vm_op_proc_enter,
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
}  

/*
  stack[0] = whole args
  ----
  code = (call kind, proc, macro call)
  stack = (arg1, arg2, ..., arg-bottom)
*/
void macro_call(Lisp_ptr proc, const ProcInfo* info){
  assert(vm.code.back().get<VMop>() == vm_op_call);

  auto args = vm.stack.back();
  vm.stack.pop_back();

  auto argc = list_to_stack("macro-call", args.get<Cons*>()->cdr(), vm.stack);

  if((argc < info->required_args)
     || (!info->variadic && argc > info->required_args)){
    for(int i = 0; i < argc; ++i){
      vm.stack.pop_back();
    }
    throw make_zs_error("macro-call error: number of passed args is mismatched!!"
                        " (required %d args, %s, passed %d)\n",
                        info->required_args,
                        (info->variadic) ? "variadic" : "not variadic",
                        argc);
  }
  vm.stack.push_back({Ptr_tag::vm_argcount, argc});

  vm.code.back() = vm_op_macro_call;
  proc_enter_entrypoint(proc); // direct jump to proc_enter()
}

/*
  stack[0] = whole args
  ----
  code = (call kind, proc)
  stack = (whole args, arg-bottom)
*/
void whole_function_call(Lisp_ptr proc){
  assert(vm.code.back().get<VMop>() == vm_op_call);

  vm.stack.push_back({Ptr_tag::vm_argcount, 1});

  vm.code.pop_back();
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
    vm.code.pop_back();
    vm.stack.pop_back();

    throw make_zs_error("eval error: (# # ...)'s first element is not procedure (got: %s)\n",
                        stringify(proc.tag()));
  }

  const ProcInfo* info = get_procinfo(proc);

  switch(info->calling){
  case Calling::function:
    return function_call(proc, info);
  case Calling::macro:
    return macro_call(proc, info);
  case Calling::whole_function:
    return whole_function_call(proc);
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

  native_func();
  // if(!vm.return_value())
  //   cerr << "eval warning: native func returned undef!\n";
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
  if(!vm.code.empty()
     && vm.code.back().get<VMop>() == vm_op_leave_frame){
    vm.code.pop_back();
    vm.leave_frame();
  }

  if(!fun->info()->early_bind){
    if(auto closure = fun->closure()){
      vm.enter_frame(closure->push());
    }else{
      vm.enter_frame(vm.frame()->push());
    }
  }

  vm.code.push_back(vm_op_leave_frame);

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
   
    vm.local_set(arg_name_cell->car().get<Symbol*>(), *i);
    arg_name = arg_name_cell->cdr();
  }

  // variadic arg push
  if(argi->variadic){
    if(!arg_name.get<Symbol*>()){
      throw zs_error("eval error: no arg name for variadic arg!\n");
    }

    auto var_argc = argc.get<int>() - static_cast<int>(std::distance(arg_start, i));
    vm.stack.erase(arg_start, i);
    vm.stack.push_back({Ptr_tag::vm_argcount, var_argc});
    vm.local_set(arg_name.get<Symbol*>(), stack_to_list<false>(vm.stack));
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

/*
  for internal direct 'goto'.
 */
void proc_enter_entrypoint(Lisp_ptr proc){
  assert(!vm.stack.empty());

  if(auto ifun = proc.get<IProcedure*>()){
    proc_enter_interpreted(ifun);
  }else if(auto nfun = proc.get<const NProcedure*>()){
    proc_enter_native(nfun);
  }else if(auto cont = proc.get<Continuation*>()){
    proc_enter_cont(cont);
  }else{
    throw zs_error("eval internal error: corrupted code stack -- no proc found for entering!\n");
  }
}
 
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
  vm.leave_frame();
}  

/*
  stack = (consequent, alternative)
  ----
  stack = ()
  code = (consequent or alternative)
*/
void vm_op_if(){
  assert(vm.code.back().get<VMop>() == vm_op_if);

  auto test_result = vm.return_value[0];

  if(test_result.get<bool>()){
    vm.code.back() = vm.stack.back();
    vm.stack.pop_back();
    vm.stack.pop_back();
  }else{
    vm.stack.pop_back();
    vm.code.back() = vm.stack.back();
    vm.stack.pop_back();
  }
}

/*
  stack = (variable name)
  ----
  stack = ()
  return-value is setted.
*/
void vm_op_set(){
  assert(vm.code.back().get<VMop>() == vm_op_set);
  vm.code.pop_back();

  auto var = vm.stack.back().get<Symbol*>();
  vm.stack.pop_back();
  if(!var){
    throw zs_error("eval error: internal error occured (set!'s varname is dismissed)\n");
  }

  vm.set(var, vm.return_value[0]);
}

/*
  stack = (variable name)
  ----
  stack = ()
  return-value is setted.
*/
void vm_op_local_set(){
  assert(vm.code.back().get<VMop>() == vm_op_local_set);
  vm.code.pop_back();

  auto var = vm.stack.back().get<Symbol*>();
  vm.stack.pop_back();
  if(!var){
    throw zs_error("eval error: internal error occured (set!'s varname is dismissed)\n");
  }

  vm.local_set(var, vm.return_value[0]);
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

void let_internal(EarlyBind early_bind){
  auto arg = pick_args_1();
  if(!arg) return;

  // skips first 'let' symbol
  auto arg_c = arg.get<Cons*>();
  assert(arg_c);

  arg = arg_c->cdr();
  if(arg.tag() != Ptr_tag::cons || nullp(arg)){
    throw make_zs_error("eval error: informal LET syntax -- (LET . <%s>).\n",
                        (nullp(arg)) ? "nil" : stringify(arg.tag()));
  }
  arg_c = arg.get<Cons*>();

  // checks named let
  Lisp_ptr name = {};

  if(arg_c->car().tag() == Ptr_tag::symbol){
    name = arg_c->car();

    arg = arg_c->cdr();
    if(arg.tag() != Ptr_tag::cons || nullp(arg)){
      throw make_zs_error("eval error: informal LET syntax -- (LET <name> . <%s>).\n",
                          (nullp(arg)) ? "nil" : stringify(arg.tag()));
    }
    
    arg_c = arg.get<Cons*>();
  }

  // picks elements
  auto binds = arg_c->car();
  auto body = arg_c->cdr();

  if(body.tag() != Ptr_tag::cons || nullp(body)){
    throw zs_error("eval error: informal syntax for LET's body!.\n");
  }

  // parses binding list
  int len = 0;

  GrowList gl_syms;
  GrowList gl_vals;

  do_list(binds,
          [&](Cons* cell) -> bool{
            auto bind = cell->car();
            if(bind.tag() != Ptr_tag::cons){
              throw make_zs_error("eval error: informal object (%s) found in let binding.\n",
                                  stringify(bind.tag()));
            }

            auto c = bind.get<Cons*>();
            if(!c){
              throw zs_error("eval error: null found in let binding.\n");
            }
                 
            ++len;

            gl_syms.push(c->car());
            gl_vals.push(c->cdr().get<Cons*>()->car());

            return true;
          },
          [&](Lisp_ptr dot_cdr){
            if(!nullp(dot_cdr)){
              throw zs_error("eval error: let binding has dot-list!\n");
            }
          });

  if(name){
    vm.enter_frame(vm.frame()->push());
  }

  auto proc = new IProcedure(body, 
                             {Calling::function, len, Variadic::f, early_bind},
                             gl_syms.extract(), vm.frame());

  if(name){
    vm.local_set(name.get<Symbol*>(), proc);
    vm.code.push_back(vm_op_leave_frame);
  }

  vm.code.insert(vm.code.end(), {vm_op_call, proc});
  vm.stack.push_back(push_cons_list({}, gl_vals.extract()));
  vm.return_value[0] = {};
}

bool is_self_evaluating(Lisp_ptr p){
  auto tag = p.tag();
  return (tag != Ptr_tag::symbol)
    && (tag != Ptr_tag::cons)
    && (tag != Ptr_tag::vm_op);
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

        // self-evaluating
      case Ptr_tag::boolean: case Ptr_tag::character:
      case Ptr_tag::i_procedure: case Ptr_tag::n_procedure:
      case Ptr_tag::number:
      case Ptr_tag::string: case Ptr_tag::vector:
      case Ptr_tag::port: case Ptr_tag::env:
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
        throw make_zs_error("eval error: unknown object appeared! (tag = %d)!\n",
                            static_cast<int>(p.tag()));
      }
    }
  }catch(const std::exception& e){
    cerr << e.what() << endl;
    vm.return_value[0] = {};
  }

  if(!vm.code.empty()){
    cerr << "eval internal warning: VM code stack is broken!\n";
    vm.code.clear();
  }
  if(!vm.stack.empty()){
    cerr << "eval internal warning: VM stack is broken! (stack has values unexpectedly.)\n";
    vm.stack.clear();
  }
}


void apply_func(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(vm.stack, args);

  if(!is_procedure(args[0])){
    throw make_zs_error("apply error: first arg is not procedure (%s)\n",
                        stringify(args[0].tag()));
  }

  // simulating function_call()
  int argc = 0;
  for(auto i = std::next(args.begin()), e = args.end(); i != e; ++i){
    if(i->tag() == Ptr_tag::cons){
      do_list(*i,
              [&](Cons* cell) -> bool {
                vm.stack.push_back(cell->car());
                ++argc;
                return true;
              },
              [&](Lisp_ptr last){
                if(!nullp(last)){
                  cerr << "native func warning: apply: passed dot list. used as arg\n";
                  vm.stack.push_back(last);
                  ++argc;
                }
              });
    }else{
      vm.stack.push_back(*i);
      ++argc;
    }
  }
  vm.stack.push_back({Ptr_tag::vm_argcount, argc});

  proc_enter_entrypoint(args[0]); // direct jump to proc_enter()
}

void func_force(){
  auto arg = pick_args_1();
  auto d = arg.get<Delay*>();
  if(!d){
    vm.return_value[0] = arg;
    return;
  }
  
  if(d->forced()){
    vm.return_value[0] = d->get();
    return;
  }

  vm.enter_frame(d->env());
  vm.stack.push_back(arg);
  vm.code.insert(vm.code.end(),
                 {vm_op_force, vm_op_leave_frame, d->get()});
}

void call_with_values(){
  auto args = pick_args<2>();

  if(!is_procedure(args[0])){
    throw make_zs_error("call-with-values error: first arg is not procedure (%s)\n",
                        stringify(args[0].tag()));
  }

  auto info = get_procinfo(args[0]);
  if(info->required_args != 0){
    throw make_zs_error("call-with-values error: first arg takes 1 or more args (%d)\n",
                        info->required_args);
  }    

  if(!is_procedure(args[1])){
    throw make_zs_error("call-with-values error: second arg is not procedure (%s)\n",
                        stringify(args[1].tag()));
  }

  // second proc call
  vm.code.insert(vm.code.end(), {args[1], vm_op_proc_enter, vm_op_move_values});

  // first proc, calling with zero args.
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  proc_enter_entrypoint(args[0]); // direct jump to proc_enter()
}

void call_cc(){
  auto args = pick_args<1>();

  if(!is_procedure(args[0])){
    throw make_zs_error("call/cc error: first arg is not procedure (%s)\n",
                        stringify(args[0].tag()));
  }

  auto info = get_procinfo(args[0]);
  if(info->required_args != 1){
    throw make_zs_error("call/cc error: first arg mush take 1 arg (%d)\n",
                        info->required_args);
  }

  auto cont = new Continuation(vm);
  vm.stack.insert(vm.stack.end(), {cont, {Ptr_tag::vm_argcount, 1}});

  proc_enter_entrypoint(args[0]); // direct jump to proc_enter()
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

void dynamic_wind(){
  auto args = pick_args<3>();

  for(auto p : args){
    if(!is_procedure(p)){
      throw make_zs_error("error: dynamic-wind: arg is not procedure (%s)\n",
                          stringify(p.tag()));
    }

    auto info = get_procinfo(p);
    if(info->required_args != 0){
      throw make_zs_error("error: dynamic-wind: first arg mush take 0 arg (%d)\n",
                          info->required_args);
    }
  }

  vm.extent.push_back({args[0], args[1], args[2]});
  vm.code.push_back(vm_op_leave_winding);

  // third proc call
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  vm.code.push_back(args[2]);
  vm.code.push_back(vm_op_save_values_and_enter);

  // second proc call
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  vm.code.push_back(args[1]);
  vm.code.push_back(vm_op_proc_enter);

  // first proc, calling with zero args.
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  proc_enter_entrypoint(args[0]); // direct jump to proc_enter()
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
  }else{
    return "unknown vm-op";
  }
}
