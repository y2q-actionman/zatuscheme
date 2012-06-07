#include <memory>
#include <cassert>

#include "eval.hh"
#include "util.hh"
#include "keyword.hh"
#include "symbol.hh"
#include "cons.hh"
#include "function.hh"
#include "printer.hh"

using namespace std;

enum class VM_op : int{
  nop = 0,
    if_,
    set_,
    funcall,
    arg_push,
    arg_bottom,
    interpreted_call,
    native_call,
    leave_frame
};

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

void eval_begin(Lisp_ptr p){
  // set up lambda body code
  stack<Lisp_ptr, vector<Lisp_ptr>> tmp;
  
  if(!do_list(p,
              [&](Cons* c) -> bool {
                tmp.push(c->car());
                return true;
              },
              [&](Lisp_ptr last_cdr) -> bool{
                if(!nullp(last_cdr)){
                  fprintf(stderr, "eval error: body has dot list!\n");
                  return false;
                }
                return true;
              })){
    VM.return_value() = {};
    return;
  }

  while(!tmp.empty()){
    VM.code().push(tmp.top());
    tmp.pop();
  }
}

void vm_op_funcall(){
  auto proc = VM.return_value();
  auto args = VM.stack().top();
  VM.stack().pop();

  if(proc.tag() != Ptr_tag::function){
    fprintf(stderr, "eval error: (# # ...)'s first element is not procedure (%s)\n",
            stringify(proc.tag()));
    VM.return_value() = {};
    return;
  }

  auto fun = proc.get<Function*>();

  VM_op call_op;

  switch(fun->type()){
  case Function::Type::interpreted:
    call_op = VM_op::interpreted_call;
    break;
  case Function::Type::native:
    call_op = VM_op::native_call;
    break;
  default:
    UNEXP_DEFAULT();
  }

  VM.code().push(Lisp_ptr(proc));
  VM.code().push(Lisp_ptr(call_op));

  const auto& argi = fun->arg_info();
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
                   || (argi.variadic && argc > argi.required_args)){
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
    VM.stack().pop();
    VM.return_value() = {};
    return;
  }
  
  VM.stack().push(Lisp_ptr(VM_op::arg_bottom));
}

void vm_op_arg_push(){
  VM.stack().push(VM.return_value());
}

void vm_op_native_call(){
  auto proc = VM.code().top();
  VM.code().pop();
  auto fun = proc.get<Function*>();

  assert(fun->type() == Function::Type::native);

  fun->get<Function::NativeFunc>()();
  if(!VM.return_value())
    fprintf(stderr, "eval warning: native func returned undef!\n");
}

void vm_op_interpreted_call(){
  auto proc = VM.code().top();
  VM.code().pop();
  auto fun = proc.get<Function*>();

  assert(fun->type() == Function::Type::interpreted);
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

  // variadic arg push
  if(argi.variadic){
    if(!arg_name.get<Symbol*>()){
      fprintf(stderr, "eval error: no arg name for variadic arg!\n");
      VM.return_value() = {};
      return;
    }

    Cons* head = new Cons;
    Cons* c = head;

    while(!VM.stack().empty()
          && (st_top = VM.stack().top()).get<VM_op>() != VM_op::arg_bottom){
      VM.stack().pop();
    
      c->rplaca(st_top);
      Cons* cc = new Cons;
      c->rplacd(Lisp_ptr(cc));
      c = cc;
    }

    c->rplaca(Cons::NIL);
    VM.local_set(arg_name.get<Symbol*>(), Lisp_ptr(head));
  }

  // clean stack
  if(VM.stack().empty()
     || VM.stack().top().get<VM_op>() != VM_op::arg_bottom){
    fprintf(stderr, "eval error: corrupted stack -- no bottom found!\n");
    VM.return_value() = {};
    return;
  }
  VM.stack().pop();
  
  // set up lambda body code
  eval_begin(fun->get<Lisp_ptr>());
}

void vm_op_leave_frame(){
  VM.leave_frame();
}  

void eval_lambda(Lisp_ptr p){
  VM.return_value() = {};

  Cons* rest = p.get<Cons*>();

  auto args = rest->car();
  if(rest->cdr().tag() != Ptr_tag::cons){
    fprintf(stderr, "eval error: lambda has invalid body!\n");
    return;
  }

  auto arg_info = parse_func_arg(args);
  if(!arg_info){
    fprintf(stderr, "eval error: lambda has invalid args!\n");
    return;
  }

  auto code = rest->cdr();
  if(!code.get<Cons*>()){
    fprintf(stderr, "eval error: lambda has no body!\n");
    return;
  }

  auto fun = new Function(code, arg_info, VM.frame());

  VM.return_value() = Lisp_ptr{fun};
  return;
}

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

  switch(len){
  case 1:
    fprintf(stderr, "eval error: informal if expr! (no test expr)\n");
    return;
  case 2: case 3: // successed
    break;
  default:
    fprintf(stderr, "eval error: informal if expr! (more than %d exprs)\n", len);
    return;
  }

  // evaluating
  VM.code().push(Lisp_ptr(VM_op::if_));
  VM.code().push(test);

  VM.stack().push(alt);
  VM.stack().push(conseq);
}

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

void vm_op_set(){
  auto var = VM.stack().top().get<Symbol*>();
  VM.stack().pop();
  if(!var){
    fprintf(stderr, "eval error: internal error occured (set!'s varname is dismissed)\n");
    return;
  }

  VM.set(var, VM.return_value());
}

void eval_define(Lisp_ptr p){
  VM.return_value() = {};

  Cons* rest = p.get<Cons*>();

  // extracting
  auto first = rest->car();

  switch(first.tag()){
  case Ptr_tag::symbol: {
    auto var = to_varname(first);
    if(!var) return;

    auto val_l = rest->cdr().get<Cons*>();
    if(!val_l){
      fprintf(stderr, "eval error: definition has empty expr!\n");
      return;
    }
    if(val_l->cdr().tag() != Ptr_tag::cons
       || val_l->cdr().get<Cons*>()){
      fprintf(stderr, "eval error: definition has extra expr!\n");
      return;
    }

    VM.code().push(Lisp_ptr{VM_op::set_});
    VM.code().push(val_l->car());
    VM.stack().push(Lisp_ptr(var));
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

    auto value = Lisp_ptr(new Function(code, arg_info, VM.frame()));
    VM.set(var, value);

    VM.return_value() = value;
    return;
  }

  default:
    fprintf(stderr, "eval error: informal define syntax!\n");
    return;
  }
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
      auto first = c->car();

      // special operator?
      if(first.tag() == Ptr_tag::symbol){
        auto sym = first.get<Symbol*>();
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
          case Keyword::quote:  VM.return_value() = r.get<Cons*>()->car(); break;
          case Keyword::lambda: eval_lambda(r); break;
          case Keyword::if_:    eval_if(r); break;
          case Keyword::set_:   eval_set(r); break;
          case Keyword::define: eval_define(r); break;
          case Keyword::begin:  eval_begin(r); break;

          case Keyword::cond:
          case Keyword::case_:
          case Keyword::and_:
          case Keyword::or_:
          case Keyword::let:
          case Keyword::let_star:
          case Keyword::letrec:
          case Keyword::do_:
          case Keyword::delay:
          case Keyword::quasiquote:
            fprintf(stderr, "eval error: '%s' is under development...\n",
                    sym->name().c_str());
            VM.return_value() = {};
            break;

          case Keyword::else_:
          case Keyword::r_arrow:
          case Keyword::unquote:
          case Keyword::unquote_splicing:
            fprintf(stderr, "eval error: '%s' cannot be used as operator!!\n",
                    sym->name().c_str());
            VM.return_value() = {};
            break;

          default:
            UNEXP_DEFAULT();
          }
          break;
        }else{
          // macro call?
          //  try to find macro-function from symbol
          //    found -> macro expansion
          //    not found -> goto function calling
          ;
        }
      }

      // procedure call?
      VM.code().push(Lisp_ptr(VM_op::funcall));
      VM.code().push(first);
      VM.stack().push(c->cdr());
      break;
    }

    case Ptr_tag::vm_op:
      switch(p.get<VM_op>()){
      case VM_op::nop:
        break;
      case VM_op::if_:
        vm_op_if();
        break;
      case VM_op::set_:
        vm_op_set();
        break;
      case VM_op::funcall:
        vm_op_funcall();
        break;
      case VM_op::arg_push:
        vm_op_arg_push();
        break;
      case VM_op::interpreted_call:
        vm_op_interpreted_call();
        break;
      case VM_op::native_call:
        vm_op_native_call();
        break;
      case VM_op::leave_frame:
        vm_op_leave_frame();
        break;
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
