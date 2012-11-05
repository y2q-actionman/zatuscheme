#include <cstdio>
#include <memory>
#include <cassert>

#include "vm.hh"
#include "eval.hh"
#include "util.hh"
#include "symbol.hh"
#include "cons.hh"
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
    vm.code.pop_back();
    vm.code.pop_back();
    vm.code.pop_back();
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


static
bool check_argcount(const char* name, int argc, const ProcInfo* info){ 
  if((argc < info->required_args)
     || (!info->variadic && argc > info->required_args)){
    fprintf(zs::err,
            "%s error: number of passed args is mismatched!!"
            " (required %d args, %s, passed %d)\n",
            name, 
            info->required_args,
            (info->variadic) ? "variadic" : "not variadic",
            argc);
    return false;
  }
  return true;
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
  vm.code.push_back(vm_op_proc_enter);

  vm.code.push_back(args_head);
  vm.code.push_back({Ptr_tag::vm_argcount, 0});
  vm.code.push_back(vm_op_arg_push);
  vm.code.push_back(args_head.get<Cons*>()->car());
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
  if(!check_argcount("macro-call", argc, info)){
    for(int i = 0; i < argc; ++i){
      vm.stack.pop_back();
    }
    vm.return_value[0] = {};
    return;
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
    fprintf(zs::err, "eval error: (# # ...)'s first element is not procedure (got: %s)\n",
            stringify(proc.tag()));
    fprintf(zs::err, "      expr: "); print(zs::err, vm.stack.back()); fputc('\n', zs::err);
    
    vm.return_value[0] = {};
    vm.code.pop_back();
    vm.stack.pop_back();
    return;
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
      fprintf(zs::err, "eval error: no arg name for variadic arg!\n");
      vm.return_value[0] = {};
      return;
    }

    vm.stack.erase(arg_start, i);
    vm.stack.push_back({Ptr_tag::vm_argcount, 
          argc.get<int>() - static_cast<int>(std::distance(arg_start, i))});
    vm.local_set(arg_name.get<Symbol*>(), stack_to_list<false>(vm.stack));
  }else{  // clean stack
    if(i != arg_end){
      fprintf(zs::err, "eval error: corrupted stack -- passed too much args!\n");
      vm.return_value[0] = {};
      return;
    }
    vm.stack.erase(arg_start, arg_end);
  }
  
  // set up lambda body code
  // list_to_stack("funcall", fun->get(), vm.code);
  vm.code.push_back(fun->get());
  vm.code.push_back(vm_op_begin); // TODO: reduce this push
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
  vm.code.push_back(values);
  vm.code.push_back(vm_op_restore_values);

  // processes 'before' windings
  for(unsigned i = wind_index; i < vm.extent.size(); ++i){
    vm.stack.push_back({Ptr_tag::vm_argcount, 0});
    vm.code.push_back(vm.extent[i].before);
    vm.code.push_back(vm_op_proc_enter);
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

  vm.code.push_back(ret_values);
  vm.code.push_back(c);
  vm.code.push_back(vm_op_replace_vm);

  // processes 'after' windings
  for(unsigned i = vm.extent.size() - 1;
      (i >= wind_index) && (static_cast<signed>(i) >= 0);
      --i){
    vm.stack.push_back({Ptr_tag::vm_argcount, 0});
    vm.code.push_back(vm.extent[i].after);
    vm.code.push_back(vm_op_proc_enter);
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
    fprintf(zs::err, "eval internal error: corrupted code stack -- no proc found for entering!\n");
    vm.return_value[0] = {};
    return;
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
  int argc = 0;

  for(auto i = vm.return_value.begin(), e = vm.return_value.end();
      i != e; ++i){
    vm.stack.push_back(*i);
    ++argc;
  }
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
    fprintf(zs::err, "eval error: internal error occured (set!'s varname is dismissed)\n");
    return;
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
    fprintf(zs::err, "eval error: internal error occured (set!'s varname is dismissed)\n");
    return;
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
    fprintf(zs::err, "eval error: internal error occured ('force' found before not a delay\n");
    return;
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
    fprintf(zs::err, "eval error: informal LET syntax -- (LET . <%s>).\n",
            (nullp(arg)) ? "nil" : stringify(arg.tag()));
    vm.return_value[0] = {};
    return;
  }
  arg_c = arg.get<Cons*>();

  // checks named let
  Lisp_ptr name = {};

  if(arg_c->car().tag() == Ptr_tag::symbol){
    name = arg_c->car();

    arg = arg_c->cdr();
    if(arg.tag() != Ptr_tag::cons || nullp(arg)){
      fprintf(zs::err, "eval error: informal LET syntax -- (LET <name> . <%s>).\n",
              (nullp(arg)) ? "nil" : stringify(arg.tag()));
      vm.return_value[0] = {};
      return;
    }
    
    arg_c = arg.get<Cons*>();
  }

  // picks elements
  auto binds = arg_c->car();
  auto body = arg_c->cdr();

  if(body.tag() != Ptr_tag::cons || nullp(body)){
    fprintf(zs::err, "eval error: informal syntax for LET's body!.\n");
    vm.return_value[0] = {};
    return;
  }

  // parses binding list
  int len = 0;

  Cons* syms_c = new Cons;
  Lisp_ptr syms = syms_c;
  Cons* syms_c_p = syms_c;

  Cons* vals_c = new Cons;
  Lisp_ptr vals = vals_c;
  Cons* vals_c_p = vals_c;

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

                auto nsc = new Cons;
                syms_c->rplaca(c->car());
                syms_c->rplacd(nsc);
                syms_c_p = syms_c;
                syms_c = nsc;

                auto nvc = new Cons;
                vals_c->rplaca(c->cdr().get<Cons*>()->car());
                vals_c->rplacd(nvc);
                vals_c_p = vals_c;
                vals_c = nvc;

                return true;
              },
              [&](Lisp_ptr dot_cdr) -> bool{
                if(!nullp(dot_cdr)){
                  return false;
                }

                if(syms_c == syms_c_p){
                  assert(vals_c == vals_c_p);
                  delete syms_c;
                  delete vals_c;
                  syms = Cons::NIL;
                  vals = Cons::NIL;
                }else{
                  delete syms_c;
                  delete vals_c;
                  syms_c_p->rplacd(Cons::NIL);
                  vals_c_p->rplacd(Cons::NIL);
                }

                return true;
              })){
    fprintf(zs::err, "eval error: let binding was failed!\n");
    free_cons_list(syms);
    free_cons_list(vals);
    vm.return_value[0] = {};
    return;
  }

  if(name){
    vm.enter_frame(vm.frame()->push());
  }

  auto proc = new IProcedure(body, 
                             {Calling::function, len, Variadic::f, early_bind},
                             syms, vm.frame());

  if(name){
    vm.local_set(name.get<Symbol*>(), proc);
    vm.code.push_back(vm_op_leave_frame);
  }

  vm.code.push_back(vm_op_call);
  vm.code.push_back(proc);
  vm.stack.push_back(push_cons_list({}, vals));
  vm.return_value[0] = {};
}

bool is_self_evaluating(Lisp_ptr p){
  auto tag = p.tag();
  return (tag != Ptr_tag::symbol)
    && (tag != Ptr_tag::cons)
    && (tag != Ptr_tag::vm_op);
}

void eval(){
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
      vm.code.pop_back();
      fprintf(zs::err, "eval error: undefined is passed!\n");
      vm.return_value[0] = {};
      break;

    case Ptr_tag::vm_argcount:
      vm.code.pop_back();
      fprintf(zs::err, "eval internal error: vm-argcount is rest on VM code stack!\n");
      vm.return_value[0] = {};
      break;

    default:
      vm.code.pop_back();
      fprintf(zs::err, "eval error: unknown object appeared! (tag = %d)!\n",
              static_cast<int>(p.tag()));
      vm.return_value[0] = {};
      break;
    }
  }

  if(!vm.stack.empty()){
    fprintf(zs::err, "eval internal warning: VM stack is broken! (stack has values unexpectedly.)\n");
    do{
      vm.stack.pop_back();
    }while(!vm.stack.empty());
  }
}


void apply_func(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(vm.stack, args);

  if(!is_procedure(args[0])){
    fprintf(zs::err, "apply error: first arg is not procedure (%s)\n",
            stringify(args[0].tag()));
    vm.return_value[0] = {};
    return;
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
                  fprintf(zs::err, "native func warning: apply: passed dot list. used as arg\n");
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

  vm.stack.push_back(arg);
  vm.code.push_back(vm_op_force);
  vm.enter_frame(d->env());
  vm.code.push_back(vm_op_leave_frame);
  vm.code.push_back(d->get());
}

void call_with_values(){
  auto args = pick_args<2>();

  if(!is_procedure(args[0])){
    fprintf(zs::err, "call-with-values error: first arg is not procedure (%s)\n",
            stringify(args[0].tag()));
    vm.return_value[0] = {};
    return;
  }

  auto info = get_procinfo(args[0]);
  if(info->required_args != 0){
    fprintf(zs::err, "call-with-values error: first arg takes 1 or more args (%d)\n",
            info->required_args);
    vm.return_value[0] = {};
    return;
  }    

  if(!is_procedure(args[1])){
    fprintf(zs::err, "call-with-values error: second arg is not procedure (%s)\n",
            stringify(args[1].tag()));
    vm.return_value[0] = {};
    return;
  }

  // second proc call
  vm.code.push_back(args[1]);
  vm.code.push_back(vm_op_proc_enter);
  vm.code.push_back(vm_op_move_values);

  // first proc, calling with zero args.
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  proc_enter_entrypoint(args[0]); // direct jump to proc_enter()
}

void call_cc(){
  auto args = pick_args<1>();

  if(!is_procedure(args[0])){
    fprintf(zs::err, "call/cc error: first arg is not procedure (%s)\n",
            stringify(args[0].tag()));
    vm.return_value[0] = {};
    return;
  }

  auto info = get_procinfo(args[0]);
  if(info->required_args != 1){
    fprintf(zs::err, "call/cc error: first arg mush take 1 arg (%d)\n",
            info->required_args);
    vm.return_value[0] = {};
    return;
  }

  auto cont = new Continuation(vm);
  vm.stack.push_back(cont);
  vm.stack.push_back({Ptr_tag::vm_argcount, 1});

  proc_enter_entrypoint(args[0]); // direct jump to proc_enter()
}

/*
*/
static void vm_op_leave_winding(){
  assert(vm.code.back().get<VMop>() == vm_op_leave_winding);
  vm.code.pop_back();

  vm.extent.pop_back();
}  

void dynamic_wind(){
  auto args = pick_args<3>();

  for(auto p : args){
    if(!is_procedure(p)){
      fprintf(zs::err, "error: dynamic-wind: arg is not procedure (%s)\n",
              stringify(p.tag()));
      vm.return_value[0] = {};
      return;
    }

    auto info = get_procinfo(p);
    if(info->required_args != 0){
      fprintf(zs::err, "error: dynamic-wind: first arg mush take 0 arg (%d)\n",
              info->required_args);
      vm.return_value[0] = {};
      return;
    }
  }

  vm.extent.push_back({args[0], args[1], args[2]});
  vm.code.push_back(vm_op_leave_winding);

  // third proc call
  vm.stack.push_back({Ptr_tag::vm_argcount, 0});
  vm.code.push_back(args[2]);
  vm.code.push_back(vm_op_proc_enter);

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
  }else{
    return "unknown vm-op";
  }
}
