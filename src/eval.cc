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
    set_
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
};

void funcall(const Function* fun, Lisp_ptr args){
  const auto& argi = fun->arg_info();
  const auto env = fun->closure();

  const auto set_arg = [&](Symbol* sym, Lisp_ptr p){
    if(env){
      VM.local_set(sym, p);
    }else{
      VM.stack().push(p);
    }
  };
      

  Lisp_ptr arg_name = argi.head;

  VM.return_value() = {};

  if(env){
    VM.enter_frame(push_frame(env));
  }

  // push args
  int argc = 0;

  if(!do_list(args,
              [&](Cons* cell) -> bool{
                assert(argc <= argi.required_args);

                if(argc == argi.required_args)
                  return false;

                VM.code().push(cell->car());
                eval();
                auto evaled = VM.return_value();
                if(!evaled){
                  fprintf(stderr, "eval error: evaluating func's arg failed!!\n");
                  return false;
                }

                auto arg_name_cell = arg_name.get<Cons*>();
                set_arg(arg_name_cell->car().get<Symbol*>(), evaled);
                arg_name = arg_name_cell->cdr();
                ++argc;

                return true;
              },
              [&](Lisp_ptr dot_cdr) -> bool{
                assert(argc <= argi.required_args);

                if(argc < argi.required_args){
                  fprintf(stderr, "eval error: internal argument counter mismatched!! (read %d args)\n",
                          argc);
                  return false;
                }

                if(argi.variadic){
                  set_arg(arg_name.get<Symbol*>(), dot_cdr);
                  ++argc;
                  return true;
                }else{
                  if(!nullp(dot_cdr)){
                    fprintf(stderr, "funcall error: argcount mismatch! (more than required %d)\n",
                            argi.required_args);
                    return false;
                  }
              
                  return true;
                }
              })){
    goto end;
  }

  // real call
  switch(fun->type()){
  case Function::Type::interpreted:
    do_list(fun->get<Lisp_ptr>(),
            [&](Cons* cell) -> bool {
              VM.code().push(cell->car());
              eval();
              return true;
            },
            [&](Lisp_ptr last_cdr){
              if(!nullp(last_cdr)){
                fprintf(stderr, "eval error: body has dot list!\n");
                VM.return_value() = {};
              }
            });
    break;
  case Function::Type::native:
    fun->get<Function::NativeFunc>()();
    if(!VM.return_value())
      fprintf(stderr, "eval error: native func returned undef!\n");
    break;
  default:
    UNEXP_DEFAULT();
  }

 end:
  if(env){
    VM.leave_frame();
  }else{
    // cleaned by native func
  }
}

void eval_lambda(const Cons* rest){
  VM.return_value() = {};

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

void eval_if(Cons* rest){
  VM.return_value() = {};

  // extracting
  Lisp_ptr test, conseq, alt;

  int len =
  bind_cons_list(Lisp_ptr(rest),
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

void eval_set(Cons* rest){
  VM.return_value() = {};

  // extracting
  Symbol* var = nullptr;
  Lisp_ptr val;

  int len =
    bind_cons_list(Lisp_ptr(rest),
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

void eval_define(const Cons* rest){
  VM.return_value() = {};

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

    //VM.code().push(Lisp_ptr{VM_op::set_});
    VM.code().push(val_l->car());
    VM.stack().push(Lisp_ptr(var));
    eval();
    vm_op_set();
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
          Cons* r = c->cdr().get<Cons*>();
          if(!r){
            fprintf(stderr, "eval error: expresssion (<KEYWORD>%s) is informal!\n",
                    (c->cdr().tag() == Ptr_tag::cons) ? "" : ". #");
            VM.return_value() = {};
            break;
          }

          switch(k){
          case Keyword::quote:  VM.return_value() = r->car(); break;
          case Keyword::lambda: eval_lambda(r); break;
          case Keyword::if_:    eval_if(r); break;
          case Keyword::set_:   eval_set(r); break;
          case Keyword::define: eval_define(r); break;

          case Keyword::cond:
          case Keyword::case_:
          case Keyword::and_:
          case Keyword::or_:
          case Keyword::let:
          case Keyword::let_star:
          case Keyword::letrec:
          case Keyword::begin:
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
      VM.code().push(first);
      eval();
      auto proc = VM.return_value();
      if(proc.tag() != Ptr_tag::function){
        fprintf(stderr, "eval error: (# # ...)'s first element is not procedure (%s)\n",
                stringify(proc.tag()));
        VM.return_value() = {};
        break;
      }

      funcall(proc.get<Function*>(), c->cdr());
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
