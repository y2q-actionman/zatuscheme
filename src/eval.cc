#include <cassert>
#include <iostream>

#include "cons.hh"
#include "cons_util.hh"
#include "equality.hh"
#include "env.hh"
#include "eval.hh"
#include "lisp_ptr.hh"
#include "procedure.hh"
#include "s_closure.hh"
#include "s_rules.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

using namespace std;
using namespace proc_flag;

bool dump_mode = false;
unsigned instruction_counter = 0;
const unsigned gc_invoke_interval = 0x100u;

namespace {

void local_set_with_identifier(Env* e, Lisp_ptr ident, Lisp_ptr value){
  e->local_set(ident, value);
}

/*
  ret = some value
*/
void vm_op_arg_push(){
  vm.stack.push_back(vm.return_value_1());
}


void eval_call(Lisp_ptr proc){
  auto args = vm.stack.back();
  vm.stack.pop_back();

  vector<Lisp_ptr> tmpv;
  for(auto i = next(begin(args)); i; ++i){ 
    tmpv.push_back(*i);
  }

  vm.code.insert(vm.code.end(), 
                 {proc, vm_op_proc_enter,
                  VMArgcount{static_cast<int>(tmpv.size())}});

  for(auto i = tmpv.rbegin(), e = tmpv.rend(); i != e; ++i){
    vm.code.insert(vm.code.end(), {vm_op_arg_push, *i});
  }
}

void vm_op_reevaluate(){
  vm.code.insert(vm.code.end(),
                 vm.return_value.begin(), vm.return_value.end());
}  

/*
  return = an, an-1, ...
  code = (this), <VMop arg push>, ..., argcount[b]
  stack = bn, bn-1, ...
  ---
  return = {}
  code = ..., argcount[a+b]
  stack = bn, bn-1, ..., an, an-1, ...
*/
void vm_op_stack_splicing(){
  if(vm.code.back().tag() != Ptr_tag::vm_op
     || vm.code.back().get<VMop>() != vm_op_arg_push){
    throw_zs_error({}, "eval internal error: stack-splicing operater is called in invalid context!\n");
  }
  vm.code.pop_back();

  // pushes return-value to vm.stack
  int argc = vm.return_value.size();
  vm.stack.insert(vm.stack.end(), vm.return_value.begin(), vm.return_value.end());

  // formatting vm.code like 'splicing is processed'
  // see vm_op_arg_push()
  for(auto i = vm.code.rbegin(), e = vm.code.rend(); i != e; i++){
    if(i->tag() == Ptr_tag::vm_argcount){
      // this '-1' negates function_call()'s estimation.
      auto new_argc = i->get<VMArgcount>() + argc - 1;
      *i = VMArgcount{new_argc};
      return;
    }
  }

  throw_zs_error({}, "eval internal error: stack-splicing cannot find corresponding vm_argcount marker!\n");
}

void quote_call(Lisp_ptr proc){
  auto args = vm.stack.back();
  vm.stack.pop_back();

  auto info = get_procinfo(proc);

  int argc = 0;
  auto i = next(begin(args));

  // required args
  for(; i && argc < info->required_args; ++i, ++argc){
    vm.stack.push_back(*i);
  }
  
  // variadic/optional args
  if(info->is_variadic()){
    vm.stack.push_back(i.base());
    ++argc;
  }else{
    for(; i; ++i, ++argc){
      vm.stack.push_back(*i);
    }
  }

  vm.stack.push_back(VMArgcount{argc});

  vm.code.insert(vm.code.end(), {proc, vm_op_proc_enter});
}

/*
  stack[0] = whole args
  ----
  code = (call kind, proc)
  stack = (whole args, arg-bottom)
*/
void whole_call(Lisp_ptr proc){
  vm.stack.insert(vm.stack.end(),
                  {vm.frame, VMArgcount{2}});
  vm.code.insert(vm.code.end(), {proc, vm_op_proc_enter});
}



void proc_enter_native(const NProcedure* fun, const ProcInfo* info, ZsArgs&& args){
  auto native_func = fun->get();
  assert(native_func);

  assert(info->required_args <= args.size() && args.size() <= info->max_args);

  try{
    auto p = native_func(move(args));

    if(info->move_ret == MoveReturnValue::t){
      vm.return_value = {p};
    }
  }catch(Lisp_ptr p){
    throw_zs_error_append(get_procname(fun), p);
  }
}

/*
  stack = (arg1, arg2, ..., <argcount>)
  ----
  In new frame, args are bound.
  code = (body1, body2, ..., leave_frame)
  stack = ()
*/
void proc_enter_interpreted(IProcedure* fun, const ProcInfo* info, ZsArgs&& args){
  // tail call check
  if(!vm.code.empty()
     && vm.code.back().tag() == Ptr_tag::vm_op
     && vm.code.back().get<VMop>() == vm_op_leave_frame){
    vm.code.pop_back();
    vm_op_leave_frame();
  }

  // == entering frame ==
  auto oldenv = vm.frame;

  auto closure = fun->closure();
  assert(closure);
  vm.frame = closure->push();

  switch(fun->info()->leaving){
  case Leaving::immediate:
    vm.code.insert(vm.code.end(), {oldenv, vm_op_leave_frame});
    break;
  case Leaving::after_returning_op:
    assert(info->returning == Returning::code
           || info->returning == Returning::stack_splice);
    assert(vm.code.back().tag() == Ptr_tag::vm_op);
    vm.code.insert(prev(vm.code.end()), {oldenv, vm_op_leave_frame});
    break;
  default:
    UNEXP_DEFAULT();
  }

  // == processing args ==
  auto arg_name_i = begin(fun->arg_list());

  const auto arg_end = args.end();
  auto arg_i = args.begin();

  // normal arg push
  for(; arg_i != arg_end && arg_name_i; ++arg_i, ++arg_name_i){
    local_set_with_identifier(vm.frame, *arg_name_i, *arg_i);
  }

  // variadic arg push
  if(info->max_args > info->required_args){
    if(!identifierp(arg_name_i.base())){
      throw_zs_error(fun, "eval error: no arg name for variadic arg!\n");
    }

    if(info->passing == Passing::quote && info->is_variadic()){
      // see 'quote_call()'
      assert(distance(arg_i, arg_end) == 1);
      assert(arg_i->tag() == Ptr_tag::cons);
      local_set_with_identifier(vm.frame, arg_name_i.base(), *arg_i);
    }else{
      local_set_with_identifier(vm.frame, arg_name_i.base(), make_cons_list(arg_i, arg_end));
    }
  }else{
    if(arg_i != arg_end || arg_name_i){
      throw_zs_error(fun, "eval error: corrupted stack -- passed too much args!\n");
    }
  }

  // cleaning stack
  args.cleanup();

  // adds procedure's name
  if(fun->name()){
    local_set_with_identifier(vm.frame, fun->name(), fun);
  }

  // set up lambda body code
  vector<Lisp_ptr> tmpv;
  for(auto j = begin(fun->get()); j; ++j)
    tmpv.push_back(*j);

  vm.code.insert(vm.code.end(), tmpv.rbegin(), tmpv.rend());
}


static unsigned get_wind_index(const VM& va, const VM& vb){
  unsigned w = 0;
  while((w < va.extent.size())
        && (w < vb.extent.size())
        && eq_internal(va.extent[w].thunk, vb.extent[w].thunk)){
    ++w;
  }
  return w;
}

void vm_op_restore_values(){
  auto values_p = vm.code.back();
  assert(values_p.tag() == Ptr_tag::vector);
  auto values = values_p.get<Vector*>();
  assert(values);
  vm.code.pop_back();

  vm.return_value = std::move(*values);
  zs_delete(values);
}

void vm_op_replace_vm(){
  auto cont = vm.code.back();
  vm.code.pop_back();
  assert(cont.tag() == Ptr_tag::continuation);

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
    vm.stack.push_back(VMArgcount{0});
    vm.code.insert(vm.code.end(), {vm.extent[i].before, vm_op_proc_enter});
  }
}

/*
  stack = (arg1, arg2, ..., arg-bottom)
  ----
  replaces VM!
*/
void proc_enter_cont(Continuation* c, ZsArgs&& args){
  auto& next_vm = c->get();

  // finds dynamic-winds for processing..
  const auto wind_index = get_wind_index(vm, next_vm);

  // saves arguments .
  // They become return-values of the passed continuation.
  auto ret_values = zs_new<Vector>(begin(args), end(args));
  args.cleanup();

  vm.code.insert(vm.code.end(), {ret_values, c, vm_op_replace_vm});

  // processes 'after' windings
  for(unsigned i = vm.extent.size() - 1;
      (i >= wind_index) && (static_cast<signed>(i) >= 0);
      --i){
    vm.stack.push_back(VMArgcount{0});
    vm.code.insert(vm.code.end(), {vm.extent[i].after, vm_op_proc_enter});
  }
}

void proc_enter_srule(SyntaxRules* srule, ZsArgs&& args){
  assert(args.size() == 2);

  check_type(Ptr_tag::env, args[1]);

  try{
    vm.return_value = {srule->apply(args[0], args[1].get<Env*>())};
  }catch(Lisp_ptr p){
    throw_zs_error_append(srule->name(), p);
  }
}

} //namespace

/*
  ret = proc
  stack[0] = args
  ---
  goto proc_call or macro_call
*/
void vm_op_call(){
  auto proc = vm.return_value_1();

  if(!is_procedure(proc)){
    // NOTE: too ad-hoc. should be rewritten.
    // We must process args, even this situation is almost wrong.
    // This is required for using a continuation in args. If no global
    // jump occured, errors are reported after.
    return eval_call(proc);
  }

  const ProcInfo* info = get_procinfo(proc);
  assert(info);

  switch(info->passing){
  case Passing::eval:
    return eval_call(proc);
  case Passing::quote:
    return quote_call(proc);
  case Passing::whole:
    return whole_call(proc);
  default:
    UNEXP_DEFAULT();
  }
}

/*
  code = (proc)
  ----
  code = ()
   and goto handler.
*/
void vm_op_proc_enter(){
  auto proc = vm.code.back();
  vm.code.pop_back();

  check_procedure_type(proc);

  assert(!vm.stack.empty());

  auto info = get_procinfo(proc);

  assert(vm.stack.back().tag() == Ptr_tag::vm_argcount);
  ZsArgs args;

  if(!(info->required_args <= args.size() && args.size() <= info->max_args)){
    throw_zs_error(get_procname(proc),
                   "number of passed args is mismatched!!"
                   " (acceptable %d-%d args, passed %d)\n",
                   info->required_args, info->max_args, args.size());
  }

  switch(info->returning){
  case Returning::pass:
    break;
  case Returning::code:
    vm.code.push_back(vm_op_reevaluate);
    break;
  case Returning::stack_splice:
    vm.code.push_back(vm_op_stack_splicing);
    break;
  default:
    UNEXP_DEFAULT();
  }

  switch(proc.tag()){
  case Ptr_tag::i_procedure:
    return proc_enter_interpreted(proc.get<IProcedure*>(), info, move(args));
  case Ptr_tag::n_procedure:
    return proc_enter_native(proc.get<const NProcedure*>(), info, move(args));
  case Ptr_tag::continuation:
    return proc_enter_cont(proc.get<Continuation*>(), move(args));
  case Ptr_tag::syntax_rules:
    return proc_enter_srule(proc.get<SyntaxRules*>(), move(args));
  case Ptr_tag::undefined: case Ptr_tag::boolean:
  case Ptr_tag::character: case Ptr_tag::cons:
  case Ptr_tag::symbol:
  case Ptr_tag::integer: case Ptr_tag::rational:
  case Ptr_tag::real:    case Ptr_tag::complex:
  case Ptr_tag::string:    case Ptr_tag::vector:
  case Ptr_tag::input_port: case Ptr_tag::output_port:
  case Ptr_tag::env:
  case Ptr_tag::syntactic_closure:
  case Ptr_tag::vm_op:
  case Ptr_tag::vm_argcount:
  case Ptr_tag::notation:
  default:
    UNEXP_DEFAULT();
  }
}
 
/*
  ret = (args)
  ----
  stack = (args)
  goto proc_enter 
*/
void vm_op_move_values(){
  int argc = vm.return_value.size();
  vm.stack.insert(vm.stack.end(), vm.return_value.begin(), vm.return_value.end());
  vm.stack.push_back(VMArgcount{argc});
}
 
/*
  leaves frame.
  no stack operations.
*/
void vm_op_leave_frame(){
  auto oldenv = vm.code.back();
  assert(oldenv.tag() == Ptr_tag::env);
  vm.code.pop_back();

  vm.frame = oldenv.get<Env*>();
}  

/*
  code = [consequent, alternative]
  ----
  stack = (consequent or alternative)
*/
void vm_op_if(){
  auto test = vm.return_value_1();

  // anything is #t, except #f and null
  auto b = (test.tag() == Ptr_tag::boolean)
    ? test.get<bool>() : test.operator bool();

  if(b){
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
  auto var = vm.code.back();
  vm.code.pop_back();

  check_identifier_type(var);

  auto val = vm.return_value_1();

  if(var.tag() == Ptr_tag::symbol){
    vm.frame->set(var, val);
  }else if(var.tag() == Ptr_tag::syntactic_closure){
    if(vm.frame->find(var).second){
      // bound alias
      vm.frame->set(var, val);
    }else{
      // not-bound syntactic closure
      auto sc = var.get<SyntacticClosure*>();
      assert(sc);

      sc->env()->set(sc->expr(), val);
    }
  }else{
    UNEXP_DEFAULT();
  }
}

void vm_op_define(){
  auto var = vm.code.back();
  vm.code.pop_back();

  check_identifier_type(var);

  auto val = vm.return_value_1();

  if(is_procedure(val) && !get_procname(val)){
    set_procname(val, var);
  }

  local_set_with_identifier(vm.frame, var, val); 
}

void vm_op_raise(){
  throw vm.return_value_1();
}

void eval(){
  while(!vm.code.empty()){
    try{
      if(dump_mode) clog << vm << endl;
      auto p = vm.code.back();
      vm.code.pop_back();

      switch(p.tag()){
      case Ptr_tag::symbol:
        vm.return_value = {vm.frame->find(p).first};
        break;
    
      case Ptr_tag::cons:
        if(auto c = p.get<Cons*>()){
          vm.code.insert(vm.code.end(), {vm_op_call, c->car});
          vm.stack.push_back(p);
        }else{
          vm.return_value = {Cons::NIL};
        }
        break;

      case Ptr_tag::vm_op: {
        auto op = p.get<VMop>();
        assert(op);
        op();
      }
        break;

      case Ptr_tag::syntactic_closure: {
        if(identifierp(p)){
          auto val = vm.frame->find(p);
          if(val.second){
            // bound alias
            vm.return_value = {val.first};
            break;
          }
        }

        // not-bound syntax closure
        auto sc = p.get<SyntacticClosure*>();
        assert(sc);

        auto newenv = sc->env()->push();
        for(auto i = begin(sc->free_names()); i; ++i){
          local_set_with_identifier(newenv, *i,
                                    vm.frame->find(*i).first);
        }

        auto oldenv = vm.frame;
        vm.frame = newenv;
        vm.code.insert(vm.code.end(),
                       {oldenv, vm_op_leave_frame, sc->expr()});
      }
        break;

        // self-evaluating
      case Ptr_tag::undefined:
      case Ptr_tag::boolean: case Ptr_tag::character:
      case Ptr_tag::i_procedure: case Ptr_tag::n_procedure:
      case Ptr_tag::integer:
      case Ptr_tag::rational:
      case Ptr_tag::real:
      case Ptr_tag::complex:
      case Ptr_tag::string: case Ptr_tag::vector:
      case Ptr_tag::input_port: case Ptr_tag::output_port:
      case Ptr_tag::env:
      case Ptr_tag::continuation:
      case Ptr_tag::syntax_rules:
        vm.return_value = {p};
        break;

      case Ptr_tag::vm_argcount:
        vm.stack.push_back(p);
        break;

      case Ptr_tag::notation:
      default:
        UNEXP_DEFAULT();
      }

      ++instruction_counter;
      if((instruction_counter % gc_invoke_interval) == 0)
        gc();
    }catch(Lisp_ptr p){
      if(vm.exception_handler.empty()){
        throw p; // going to std::terminate (or handlers established by debug-build)
      }

      auto handler = vm.exception_handler.back();
      vm.exception_handler.pop_back();

      vm.stack.insert(vm.stack.end(), {p, VMArgcount{1}});
      vm.code.insert(vm.code.end(), {vm_op_raise, handler, vm_op_proc_enter});
    }
  }

  if(!vm.code.empty()){
    print_zs_warning("eval internal warning: VM code stack is broken!");
    vm.code.clear();
  }

  if(!vm.stack.empty()){
    print_zs_warning("eval internal warning: VM stack is broken! (stack has values unexpectedly.)");
    vm.stack.clear();
  }
}

const char* stringify(VMop op){
  if(op == vm_op_arg_push){
    return "arg push";
  }else if(op == vm_op_reevaluate){
    return "reevaluate";
  }else if(op == vm_op_call){
    return "call";
  }else if(op == vm_op_leave_frame){
    return "leave frame";
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
  }else if(op == vm_op_define){
    return "define";
  }else if(op == vm_op_stack_splicing){
    return "splicing args";
  }else if(op == vm_op_raise){
    return "raise";
  }else{
    return "unknown vm-op";
  }
}
