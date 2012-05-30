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
  nop = 0
};

namespace {

Lisp_ptr funcall(const Function* fun, Lisp_ptr args){
  const auto& argi = fun->arg_info();
  const auto env = fun->closure();

  const auto set_arg = [&](Symbol* sym, Lisp_ptr p){
    if(env){
      VM.local_set(sym, p);
    }else{
      VM.arg_push(p);
    }
  };
      

  Lisp_ptr arg_name = argi.head;
  Lisp_ptr ret = {};

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
                auto evaled = eval();
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
              ret = eval();
              return true;
            },
            [&](Lisp_ptr last_cdr){
              if(!nullp(last_cdr)){
                fprintf(stderr, "eval error: body has dot list!\n");
                ret = Lisp_ptr{};
              }
            });
    break;
  case Function::Type::native:
    ret = (fun->get<Function::NativeFunc>())();
    if(!ret)
      fprintf(stderr, "eval error: native func returned undef!\n");
    break;
  default:
    UNEXP_DEFAULT();
  }

 end:
  if(env){
    VM.leave_frame();
  }else{
    VM.arg_clear();
  }

  return ret;
}

Lisp_ptr eval_lambda(const Cons* rest){
  auto args = rest->car();
  if(rest->cdr().tag() != Ptr_tag::cons){
    fprintf(stderr, "eval error: lambda has invalid body!\n");
    return {};
  }

  auto arg_info = parse_func_arg(args);
  if(!arg_info){
    fprintf(stderr, "eval error: lambda has invalid args!\n");
    return {};
  }

  auto code = rest->cdr();
  if(!code.get<Cons*>()){
    fprintf(stderr, "eval error: lambda has no body!\n");
    return {};
  }

  auto fun = new Function(code, arg_info, VM.frame());

  return Lisp_ptr{fun};
}

Lisp_ptr eval_if(const Cons* rest){
  // extracting
  auto test = rest->car();

  if(rest->cdr().tag() != Ptr_tag::cons){
    fprintf(stderr, "eval error: if has invalid conseq!\n");
    return {};
  }

  auto conseq_l = rest->cdr().get<Cons*>();
  if(!conseq_l){
    fprintf(stderr, "eval error: if has no conseq!\n");
    return {};
  }

  auto conseq = conseq_l->car();

  if(conseq_l->cdr().tag() != Ptr_tag::cons){
    fprintf(stderr, "eval error: if has invalid alt!\n");
    return {};
  }

  auto alt = Lisp_ptr{};

  if(auto alt_l = conseq_l->cdr().get<Cons*>()){
    alt = alt_l->car();

    if(alt_l->cdr().tag() != Ptr_tag::cons
       || alt_l->cdr().get<Cons*>()){
      fprintf(stderr, "eval error: if has extra alts!\n");
      return {};
    }
  }

  // evaluating
  VM.code().push(test);
  auto test_evaled = eval();
  if(!test_evaled){
    return {};
  }else if(test_evaled.get<bool>()){
    VM.code().push(conseq);
    return eval();
  }else if(alt){
    VM.code().push(alt);
    return eval();
  }else{
    return {};
  }
}

Lisp_ptr eval_set(const Cons* rest){
  // extracting
  auto var = rest->car().get<Symbol*>();
  if(!var){
    fprintf(stderr, "eval error: set!'s first element is not symbol!\n");
    return {};
  }
  if(to_keyword(var->name().c_str()) != Keyword::not_keyword){
    fprintf(stderr, "eval error: set!'s first element is Keyword (%s)!\n",
            var->name().c_str());
    return {};
  }

  if(rest->cdr().tag() != Ptr_tag::cons){
    fprintf(stderr, "eval error: set!'s value form is informal!\n");
    return {};
  }

  auto valp = rest->cdr().get<Cons*>();
  if(!valp){
    fprintf(stderr, "eval error: set! has no value!\n");
    return {};
  }

  if(valp->cdr().tag() != Ptr_tag::cons
     || valp->cdr().get<Cons*>()){
    fprintf(stderr, "eval error: set! has extra forms!\n");
    return {};
  }

  VM.code().push(valp->car());
  auto val = eval();

  // evaluating
  //   fprintf(stderr, "eval error: set! value is not defined previously!\n");
  VM.set(var, val);
      
  return val;
}

Lisp_ptr eval_define(const Cons* rest){
  static constexpr auto test_varname = [](Symbol* var) -> bool{
    if(!var){
      fprintf(stderr, "eval error: defined variable name is not a symbol!\n");
      return false;
    }

    if(to_keyword(var->name().c_str()) != Keyword::not_keyword){
      fprintf(stderr, "eval error: define's first element is Keyword (%s)!\n",
              var->name().c_str());
      return false;
    }

    return true;
  };

  Symbol* var = nullptr;
  Lisp_ptr value;
  unique_ptr<Function> func_value;

  // extracting
  auto first = rest->car();

  switch(first.tag()){
  case Ptr_tag::symbol: {
    var = first.get<Symbol*>();
    if(!test_varname(var))
      return {};

    auto val_l = rest->cdr().get<Cons*>();
    if(!val_l){
      fprintf(stderr, "eval error: definition has empty expr!\n");
      return {};
    }
    if(val_l->cdr().tag() != Ptr_tag::cons
       || val_l->cdr().get<Cons*>()){
      fprintf(stderr, "eval error: definition has extra expr!\n");
      return {};
    }

    VM.code().push(val_l->car());
    value = eval();
  }
    break;

  case Ptr_tag::cons: {
    auto lis = first.get<Cons*>();
    if(!lis){
      fprintf(stderr, "eval error: defined variable is not found!\n");
      return {};
    }

    var = lis->car().get<Symbol*>();
    if(!test_varname(var))
      return {};

    const auto& arg_info = parse_func_arg(lis->cdr());
    if(!arg_info){
      fprintf(stderr, "eval error: defined function argument is informal!\n");
      return {};
    }
    
    auto code = rest->cdr();
    if(!code.get<Cons*>()){
      fprintf(stderr, "eval error: definition has empty body!\n");
      return {};
    }

    func_value = unique_ptr<Function>(new Function(code, arg_info, VM.frame()));
    value = Lisp_ptr(func_value.get());
  }
    break;

  default:
    fprintf(stderr, "eval error: informal define syntax!\n");
    return {};
  }

  // assignment
  VM.set(var, value);

  func_value.release();
  return value;
}

} // namespace

Lisp_ptr eval(){
  auto p = VM.code().top();
  VM.code().pop();

  switch(p.tag()){
  case Ptr_tag::symbol: {
    auto sym = p.get<Symbol*>();
    if(to_keyword(sym->name().c_str()) != Keyword::not_keyword){
      fprintf(stderr, "eval error: symbol '%s' is keyword!!\n", sym->name().c_str());
      return {};
    }

    return VM.find(sym);
  }
    
  case Ptr_tag::cons: {
    auto c = p.get<Cons*>();
    auto first = c->car();

    // special operator?
    if(first.tag() == Ptr_tag::symbol){
      auto sym = first.get<Symbol*>();
      auto k = to_keyword(sym->name().c_str());

      if(k != Keyword::not_keyword){
        Cons* r = c->cdr().get<Cons*>();
        if(!r){
          fprintf(stderr, "eval error: expresssion (<KEYWORD>%s) is informal!\n",
                  (c->cdr().tag() == Ptr_tag::cons) ? "" : ". #");
          return {};
        }

        switch(k){
        case Keyword::quote:  return r->car();
        case Keyword::lambda: return eval_lambda(r);
        case Keyword::if_:    return eval_if(r);
        case Keyword::set_:   return eval_set(r);

        case Keyword::cond:   return {}; // under development
        case Keyword::case_:  return {}; // under development
        case Keyword::and_:   return {}; // under development
        case Keyword::or_:    return {}; // under development
        case Keyword::let:    return {}; // under development
        case Keyword::let_star: return {}; // under development
        case Keyword::letrec: return {}; // under development
        case Keyword::begin:  return {}; // under development
        case Keyword::do_:    return {}; // under development
        case Keyword::delay:  return {}; // under development
        case Keyword::quasiquote: return {}; // under development

        case Keyword::define: return eval_define(r);

        case Keyword::else_:
        case Keyword::r_arrow:
        case Keyword::unquote:
        case Keyword::unquote_splicing:
          fprintf(stderr, "eval error: '%s' cannot be used as operator!!\n",
                  sym->name().c_str());
          return {};

        default:
          UNEXP_DEFAULT();
        }
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
    auto proc = eval();
    if(proc.tag() != Ptr_tag::function){
      fprintf(stderr, "eval error: (# # ...)'s first element is not procedure (%s)\n",
              stringify(proc.tag()));
      return {};
    }

    return funcall(proc.get<Function*>(), c->cdr());
  }

  case Ptr_tag::vm_op:
    switch(p.get<VM_op>()){
    case VM_op::nop:
      break;
    default:
      UNEXP_DEFAULT();
    }
    
  default: // almost self-evaluating
    if(!p){
      fprintf(stderr, "eval error: undefined value passed!!\n");
    }
    return p;
  }
}
