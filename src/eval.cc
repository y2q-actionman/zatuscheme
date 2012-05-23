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

namespace {

Lisp_ptr funcall(const Function* fun, Lisp_ptr args){
  const auto& argi = fun->arg_info();
  Lisp_ptr arg_name = argi.head;
  Lisp_ptr ret = {};

  // push args
  int argc = 0;

  VM.enter_frame(fun->get_closure());

  if(!do_list(args,
              [&](Cons* cell) -> bool{
                assert(argc <= argi.required_args);

                if(argc == argi.required_args)
                  return false;

                auto evaled = eval(cell->car());
                if(!evaled){
                  fprintf(stderr, "eval error: evaluating func's arg failed!!\n");
                  return false;
                }

                auto arg_name_cell = arg_name.get<Cons*>();
                VM.set(arg_name_cell->car().get<Symbol*>(), evaled);
                VM.arg_push(evaled);
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
                  VM.set(arg_name.get<Symbol*>(), dot_cdr);
                  VM.arg_push(dot_cdr);
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
              ret = eval(cell->car());
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
  VM.arg_clear();
  VM.leave_frame();
  
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

  auto fun = new Function(code, arg_info);
  enclose(code, fun->get_closure());

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
  auto test_evaled = eval(test);
  if(!test_evaled){
    return {};
  }else if(test_evaled.get<bool>()){
    return eval(conseq);
  }else{
    return alt ? eval(alt) : Lisp_ptr{};
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

  auto val = eval(valp->car());

  // evaluating
  if(!VM.set(var, val)){
    fprintf(stderr, "eval error: set! value is not defined previously!\n");
  }
      
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

    value = eval(val_l->car());
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

    func_value = unique_ptr<Function>(new Function(code, arg_info));
    enclose(code, func_value->get_closure());
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

Lisp_ptr eval(Lisp_ptr p){
  if(!p){
    fprintf(stderr, "eval error: undefined value passed!!\n");
    return {};
  }

  switch(p.tag()){
  case Ptr_tag::boolean:
  case Ptr_tag::character:
  case Ptr_tag::function:
  case Ptr_tag::number:
  case Ptr_tag::string:
  case Ptr_tag::vector:
  case Ptr_tag::port:
    return p; // self-evaluating

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
    auto proc = eval(first);
    if(proc.tag() != Ptr_tag::function){
      fprintf(stderr, "eval error: (# # ...)'s first element is not procedure!!\n");
      return {};
    }

    return funcall(proc.get<Function*>(), c->cdr());
  }
    
  default:
    UNEXP_DEFAULT();
  }
}

void enclose(Lisp_ptr p, VM_t::Env& e){
  if(!p) return;

  switch(p.tag()){
  case Ptr_tag::boolean:
  case Ptr_tag::character:
  case Ptr_tag::function:
  case Ptr_tag::number:
  case Ptr_tag::string:
  case Ptr_tag::port:
    return;

  case Ptr_tag::symbol: {
    auto sym = p.get<Symbol*>();
    if(to_keyword(sym->name().c_str()) == Keyword::not_keyword){
      auto val = VM.find(sym);
      if(val) e.insert(make_pair(sym, val));
    }

    return;
  }
    
  case Ptr_tag::vector: {
    auto vect = p.get<Vector*>();
    assert(vect);

    for(auto i : *vect){
      enclose(i, e);
    }
    return;
  }

  case Ptr_tag::cons: {
    const auto enclose_list = [&](Lisp_ptr pp){
      do_list(pp,
              [&](Cons* c) -> bool {
                enclose(c->car(), e);
                return true;
              },
              [&](Lisp_ptr last){
                enclose(last, e);
              });
    };

    auto c = p.get<Cons*>();
    if(!c) return;

    auto first = c->car();

    // special operator has local bind..
    if(first.tag() == Ptr_tag::symbol){
      auto sym = first.get<Symbol*>();
      auto k = to_keyword(sym->name().c_str());

      if(k != Keyword::not_keyword){
        Cons* r = c->cdr().get<Cons*>();
        if(!r) return;

        switch(k){
        case Keyword::quote:
          return;

        case Keyword::if_:
        case Keyword::and_:
        case Keyword::or_:
        case Keyword::begin:
        case Keyword::delay:
        case Keyword::cond:
        case Keyword::case_:
        case Keyword::else_:
          enclose_list(c->cdr());
          return;

        case Keyword::lambda:
        case Keyword::set_:
        case Keyword::define:
          enclose_list(r->cdr());
          return;

        case Keyword::let:
        case Keyword::let_star:
        case Keyword::letrec:
        case Keyword::do_:
          //enclose binding list
          do_list(r->car(),
                  [&](Cons* cc) -> bool {
                    enclose_list(cc->car().get<Cons*>()->cdr());
                    return true;
                  },
                  [](Lisp_ptr){});
          //enclose body
          enclose_list(r->cdr());
          return;

        case Keyword::r_arrow: return; // only ignored?

          // under development..
        case Keyword::quasiquote:
        case Keyword::unquote:
        case Keyword::unquote_splicing:
          return;

        default:
          UNEXP_DEFAULT();
        }
      }else{
        // macro call?
      }
    }

    // procedure call -- enclose all
    enclose_list(p);

    return;
  }
    
  default:
    UNEXP_DEFAULT();
  }
}
