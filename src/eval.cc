#include <memory>
#include <cassert>

#include "eval.hh"
#include "util.hh"
#include "keyword.hh"
#include "symbol.hh"
#include "cons.hh"
#include "function.hh"
#include "printer.hh"
#include "builtin.hh"

using namespace std;

namespace {

inline
Symbol* to_varname(Lisp_ptr p){
  Symbol* var = p.get<Symbol*>();

  if(!var){
    fprintf(stderr, "eval error: variable's name is not a symbol!\n");
    return nullptr;
  }

  if(var->to_keyword() != Keyword::not_keyword){
    fprintf(stderr, "eval error: variable's name is Keyword (%s)!\n",
            var->name().c_str());
    return nullptr;
  }

  return var;
}

template<typename StackT>
void list_to_stack(const char* opname, Lisp_ptr l, StackT& st){
  stack<Lisp_ptr, vector<Lisp_ptr>> tmp;
  
  do_list(l,
          [&](Cons* c) -> bool {
            tmp.push(c->car());
            return true;
          },
          [&](Lisp_ptr last_cdr){
            if(!nullp(last_cdr)){
              fprintf(stderr, "eval warning: dot list has read as proper list. (in %s)\n",
                      opname);
              tmp.push(last_cdr);
            }
          });

  while(!tmp.empty()){
    st.push(tmp.top());
    tmp.pop();
  }
}  

/*
  ----
  code = (body1, body2, ...)
*/
void eval_begin(Lisp_ptr p){
  list_to_stack("begin", p, VM.code());
}

/*
  stack[0] = args
  ----
  code = (argN, arg-move, argN-1, arg-move, ..., call kind, proc)
  stack = (arg-bottom)
*/
void proc_call(Function* proc, VM_op call_op){
  auto args = VM.stack().top();
  VM.stack().pop();

  VM.code().push(Lisp_ptr(proc));
  VM.code().push(Lisp_ptr(call_op));

  const auto& argi = proc->arg_info();
  int argc = 0;

  if(!do_list(args,
              [&](Cons* cell) -> bool{
                VM.code().push(Lisp_ptr(VM_op::arg_push));
                VM.code().push(cell->car());
                ++argc;

                return true;
              },
              [&](Lisp_ptr dot_cdr) -> bool{
                if(!nullp(dot_cdr)){
                  fprintf(stderr, "funcall error: arg-list is dot-list!\n");
                  return false;
                }
              
                if((argc < argi.required_args)
                   || (!argi.variadic && argc > argi.required_args)){
                  fprintf(stderr, "funcall error: number of passed args is mismatched!! (required %d args, %s, passed %d)\n",
                          argi.required_args,
                          (argi.variadic) ? "variadic" : "not variadic",
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
  
  VM.stack().push(Lisp_ptr(VM_op::arg_bottom));
}

/*
  stack[0] = args
  ----
  code = (call kind, proc, macro call)
  stack = (arg1, arg2, ..., arg-bottom)
*/
void macro_call(Function* proc, VM_op call_op){
  auto args = VM.stack().top();
  VM.stack().pop();

  VM.code().push(Lisp_ptr(VM_op::macro_call));
  VM.code().push(Lisp_ptr(proc));
  VM.code().push(Lisp_ptr(call_op));

  VM.stack().push(Lisp_ptr(VM_op::arg_bottom));
  list_to_stack("macro-call", args, VM.stack());
}


/*
  ret = proc
  stack[0] = args
  ---
  goto proc_call or macro_call
 */
void vm_op_call(){
  auto proc = VM.return_value();
  if(proc.tag() != Ptr_tag::function){
    fprintf(stderr, "eval error: (# # ...)'s first element is not procedure (%s)\n",
            stringify(proc.tag()));
    VM.return_value() = {};
    return;
  }

  auto fun = proc.get<Function*>();

  switch(fun->type()){
  case Function::Type::interpreted:
    proc_call(fun, VM_op::interpreted_call);
    return;
  case Function::Type::interpreted_macro:
    macro_call(fun, VM_op::interpreted_call);
    return;
  case Function::Type::native:
    proc_call(fun, VM_op::native_call);
    return;
  case Function::Type::native_macro:
    macro_call(fun, VM_op::native_call);
    return;
  default:
    UNEXP_DEFAULT();
  }
}

/*
  ret = some value
  ----
  stack[0] = some value
*/
void vm_op_arg_push(){
  VM.stack().push(VM.return_value());
}

/*
  ret = list
  ----
  stack = (list[0], list[1], ...)
*/
void vm_op_arg_push_list(){
  list_to_stack("unquote-splicing", VM.return_value(), VM.stack());
}

/*
  code = (proc)
  stack = (arg1, arg2, ..., arg-bottom)
  ----
  ret = returned value
  code = ()
  stack = ()
*/
void vm_op_native_call(){
  auto proc = VM.code().top();
  VM.code().pop();
  auto fun = proc.get<Function*>();

  assert(fun->type() == Function::Type::native
         || fun->type() == Function::Type::native_macro);

  auto native_func = fun->get<Function::NativeFunc>();
  assert(native_func);

  native_func();
  if(!VM.return_value())
    fprintf(stderr, "eval warning: native func returned undef!\n");
}

/*
  code = (proc)
  stack = (arg1, arg2, ..., arg-bottom)
  ----
  In new frame, args are bound.
  code = (body1, body2, ..., leave_frame)
  stack = ()
*/
void vm_op_interpreted_call(){
  auto proc = VM.code().top();
  VM.code().pop();
  auto fun = proc.get<Function*>();

  assert(fun->type() == Function::Type::interpreted
         || fun->type() == Function::Type::interpreted_macro);
  assert(!VM.stack().empty());

  const auto& argi = fun->arg_info();

  // tail call check
  if(!VM.code().empty()
     && VM.code().top().get<VM_op>() == VM_op::leave_frame){
    VM.code().pop();
    VM.leave_frame();
  }

  VM.enter_frame(push_frame(fun->closure()));
  VM.code().push(Lisp_ptr(VM_op::leave_frame));

  Lisp_ptr arg_name = argi.head;
  Lisp_ptr st_top;

  // normal arg push
  while((st_top = VM.stack().top()).get<VM_op>() != VM_op::arg_bottom){
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

    stack_to_list(false);

    VM.local_set(arg_name.get<Symbol*>(), VM.return_value());
  }else{  // clean stack
    if(VM.stack().empty()
       || VM.stack().top().get<VM_op>() != VM_op::arg_bottom){
      fprintf(stderr, "eval error: corrupted stack -- no bottom found!\n");
      VM.return_value() = {};
      return;
    }
    VM.stack().pop();
  }
  
  // set up lambda body code
  eval_begin(fun->get<Lisp_ptr>());
}

/*
  leaves frame.
  no stack operations.
*/
void vm_op_leave_frame(){
  VM.leave_frame();
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
  ret = proc.
  no stack operations.
*/
void eval_lambda(Lisp_ptr p){
  Function::ArgInfo arg_info;
  Lisp_ptr code;

  bind_cons_list(p,
                 [&](Cons* c){
                   arg_info = parse_func_arg(c->car());
                   code = c->cdr();
                 });

  if(!arg_info){
    fprintf(stderr, "eval error: lambda has invalid args!\n");
    VM.return_value() = {};
    return;
  }
  if(!code){
    fprintf(stderr, "eval error: lambda has invalid body!\n");
    VM.return_value() = {};
    return;
  }

  VM.return_value() = 
    Lisp_ptr{new Function(code, Function::Type::interpreted, arg_info, VM.frame())};
}

/*
  ----
  code = (test, VM::if)
  stack = (consequent, alternative)
*/
void eval_if(Lisp_ptr p){
  VM.return_value() = {};

  // extracting
  Lisp_ptr test, conseq, alt;

  int len =
  bind_cons_list(p,
                 [&](Cons* c){
                   test = c->car();
                 },
                 [&](Cons* c){
                   conseq = c->car();
                 },
                 [&](Cons* c){
                   alt = c->car();
                 });

  if(len < 2){
    fprintf(stderr, "eval error: informal if expr! (only %d exprs)\n", len);
    return;
  }else if(len > 3){
    fprintf(stderr, "eval error: informal if expr! (more than %d exprs)\n", len);
    return;
  }

  // evaluating
  VM.code().push(Lisp_ptr(VM_op::if_));
  VM.code().push(test);

  VM.stack().push(alt);
  VM.stack().push(conseq);
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
  ----
  code = (value, VM::if)
  stack = (variable name)
*/
void eval_set(Lisp_ptr p){
  VM.return_value() = {};

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
    fprintf(stderr, "eval error: no value is supplied for set!\n");
    return;
  }

  if(len > 2){
    fprintf(stderr, "eval error: informal set! expr! (more than %d exprs)\n", len);
    return;
  }

  // evaluating
  VM.code().push(Lisp_ptr{VM_op::set_});
  VM.code().push(val);
  VM.stack().push(Lisp_ptr{var});
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
  in variable set:
    same as "set!"

  in function definition:
    immediately the function is set to the variable.
*/
void eval_define(Lisp_ptr p){
  VM.return_value() = {};

  Cons* rest = p.get<Cons*>();

  // extracting
  auto first = rest->car();

  switch(first.tag()){
  case Ptr_tag::symbol: {
    if(!to_varname(first)) return;

    Lisp_ptr val;

    auto len = bind_cons_list(rest->cdr(),
                              [&](Cons* c){
                                val = c->car();
                              });

    if(len < 1){
      fprintf(stderr, "eval error: definition has empty expr!\n");
      return;
    }else if(len > 1){
      fprintf(stderr, "eval error: definition has extra expr!\n");
      return;
    }

    VM.code().push(Lisp_ptr{VM_op::set_});
    VM.code().push(val);
    VM.stack().push(first);
    return;
  }

  case Ptr_tag::cons: {
    auto lis = first.get<Cons*>();
    if(!lis){
      fprintf(stderr, "eval error: defined variable is not found!\n");
      return;
    }

    auto var = to_varname(lis->car());
    if(!var) return;

    const auto& arg_info = parse_func_arg(lis->cdr());
    if(!arg_info){
      fprintf(stderr, "eval error: defined function argument is informal!\n");
      return;
    }
    
    auto code = rest->cdr();
    if(!code.get<Cons*>()){
      fprintf(stderr, "eval error: definition has empty body!\n");
      return;
    }

    auto value = Lisp_ptr(new Function(code, Function::Type::interpreted, arg_info, VM.frame()));
    VM.set(var, value);

    VM.return_value() = value;
    return;
  }

  default:
    fprintf(stderr, "eval error: informal define syntax!\n");
    return;
  }
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
  static const auto qq_elem = [](Lisp_ptr p){
    if(auto l = p.get<Cons*>()){
      if(auto l_first_sym = l->car().get<Symbol*>()){
        switch(l_first_sym->to_keyword()){
        case Keyword::unquote:
          VM.code().push(Lisp_ptr(VM_op::arg_push));
          VM.code().push(l->cdr().get<Cons*>()->car());
          return;
        case Keyword::unquote_splicing:
          VM.code().push(Lisp_ptr(VM_op::arg_push_list));
          VM.code().push(l->cdr().get<Cons*>()->car());
          return;
        default:
          break;
        }
      }
    }

    VM.code().push(Lisp_ptr(VM_op::arg_push));
    VM.code().push(p);
    VM.code().push(Lisp_ptr(VM_op::quasiquote));
  };

  auto p = VM.code().top();
  VM.code().pop();

  switch(p.tag()){
  case Ptr_tag::cons: {
    if(nullp(p)){
      VM.return_value() = Cons::NIL;
      return;
    }

    // check unquote -- like `,x
    if(auto first_sym = p.get<Cons*>()->car().get<Symbol*>()){
      switch(first_sym->to_keyword()){
      case Keyword::unquote: {
        auto rest = p.get<Cons*>()->cdr().get<Cons*>()->car();
        VM.code().push(rest);
        return;
      }
      case Keyword::unquote_splicing:
        fprintf(stderr, "eval error: unquote-splicing is not supported out of list");
        VM.return_value() = {};
        return;
      default:
        break;
      }
    }

    // generic lists
    VM.stack().push(Lisp_ptr(VM_op::arg_bottom));
    VM.code().push(Lisp_ptr(VM_op::quasiquote_list));

    do_list(p,
            [](Cons* c) -> bool {
              qq_elem(c->car());
              return true;
            },
            [](Lisp_ptr last){
              qq_elem(last);
            });

    return;
  }
  case Ptr_tag::vector: {
    VM.stack().push(Lisp_ptr(VM_op::arg_bottom));
    VM.code().push(Lisp_ptr(VM_op::quasiquote_vector));

    auto v = p.get<Vector*>();
    for(auto i = begin(*v); i != end(*v); ++i){
      qq_elem(*i);
    }

    return;
  }
  default:
    VM.return_value() = p;
    return;
  }
}

void vm_op_quasiquote_list(){
  stack_to_list(true);
}

void vm_op_quasiquote_vector(){
  stack_to_vector();
}

} // namespace

void eval(){
  while(!VM.code().empty()){
    auto p = VM.code().top();
    VM.code().pop();

    switch(p.tag()){
    case Ptr_tag::symbol: {
      auto sym = to_varname(p);
      if(!sym){
        VM.return_value() = {};
        break;
      }
    
      VM.return_value() = VM.find(sym);
      break;
    }
    
    case Ptr_tag::cons: {
      auto c = p.get<Cons*>();
      if(!c){
        VM.return_value() = Cons::NIL;
        break;
      }

      auto first = c->car();

      // special operator?
      if(auto sym = first.get<Symbol*>()){
        auto k = sym->to_keyword();

        if(k != Keyword::not_keyword){
          Lisp_ptr r = c->cdr();
          if(!r.get<Cons*>()){
            fprintf(stderr, "eval error: expresssion (<KEYWORD>%s) is informal!\n",
                    (r.tag() == Ptr_tag::cons) ? "" : ". #");
            VM.return_value() = {};
            break;
          }

          switch(k){
          case Keyword::quote:  
            VM.return_value() = r.get<Cons*>()->car();
            break;
          case Keyword::lambda: eval_lambda(r); break;
          case Keyword::if_:    eval_if(r); break;
          case Keyword::set_:   eval_set(r); break;
          case Keyword::define: eval_define(r); break;
          case Keyword::begin:  eval_begin(r); break;
          case Keyword::quasiquote: 
            VM.code().push(r.get<Cons*>()->car());
            VM.code().push(Lisp_ptr(VM_op::quasiquote));
            break;

          case Keyword::cond:
          case Keyword::case_:
          case Keyword::and_:
          case Keyword::or_:
          case Keyword::let:
          case Keyword::let_star:
          case Keyword::letrec:
          case Keyword::do_:
          case Keyword::delay:
            fprintf(stderr, "eval error: '%s' is under development...\n",
                    sym->name().c_str());
            VM.return_value() = {};
            break;

          case Keyword::unquote:
          case Keyword::unquote_splicing:
            VM.return_value() = p;
            break;

          case Keyword::else_:
          case Keyword::r_arrow:
            fprintf(stderr, "eval error: '%s' cannot be used as operator!!\n",
                    sym->name().c_str());
            VM.return_value() = {};
            break;

          default:
            UNEXP_DEFAULT();
          }
          break;
        }
      }

      // procedure/macro call?
      VM.code().push(Lisp_ptr(VM_op::call));
      VM.code().push(first);
      VM.stack().push(c->cdr());
      break;
    }

    case Ptr_tag::vm_op:
      switch(p.get<VM_op>()){
      case VM_op::nop:      break;
      case VM_op::if_:      vm_op_if(); break;
      case VM_op::set_:     vm_op_set(); break;
      case VM_op::call:     vm_op_call(); break;
      case VM_op::arg_push: vm_op_arg_push(); break;
      case VM_op::arg_push_list: vm_op_arg_push_list(); break;
      case VM_op::interpreted_call: vm_op_interpreted_call(); break;
      case VM_op::native_call:      vm_op_native_call(); break;
      case VM_op::leave_frame:      vm_op_leave_frame(); break;
      case VM_op::macro_call:       vm_op_macro_call(); break;
      case VM_op::quasiquote:        vm_op_quasiquote(); break;
      case VM_op::quasiquote_list:    vm_op_quasiquote_list(); break;
      case VM_op::quasiquote_vector:  vm_op_quasiquote_vector(); break;
      default:
        UNEXP_DEFAULT();
      }
      break;
    
    default: // almost self-evaluating
      VM.return_value() = p;
      break;
    }
  }
}
