#include <memory>
#include <cassert>

#include "eval.hh"
#include "util.hh"
#include "keyword.hh"
#include "symbol.hh"
#include "cons.hh"
#include "env.hh"
#include "stack.hh"
#include "function.hh"
#include "printer.hh"

using namespace std;

namespace {

Lisp_ptr funcall(const Function* fun, Env& e, Stack& s, Lisp_ptr args){
  const auto& argi = fun->arg_info();
  Lisp_ptr arg_name = argi.head;
  Lisp_ptr ret = {};

  // push args
  int argc = 0;

  if(!do_list(args,
              [&](Cons* cell) -> bool{
                assert(argc <= argi.required_args);

                if(argc == argi.required_args)
                  return false;

                auto evaled = eval(cell->car(), e, s);
                if(!evaled){
                  fprintf(stderr, "eval error: evaluating func's arg failed!!\n");
                  return false;
                }

                auto arg_name_cell = arg_name.get<Cons*>();
                s.push(arg_name_cell->car().get<Symbol*>(), evaled);
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
                  s.push(arg_name.get<Symbol*>(), dot_cdr);
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
  case Function::Type::interpreted: {
    auto code_head = fun->get<Lisp_ptr>();
    // TODO: use multiple line!
    ret = eval(code_head.get<Cons*>()->car(), e, s);
    break;
  }
  case Function::Type::native:
    ret = (fun->get<Function::NativeFunc>())(e, s);
    break;
  default:
    UNEXP_DEFAULT();
  }

  // pop args
 end:
  s.pop(argc);
  
  return ret;
}

Lisp_ptr eval_lambda(const Cons* rest, Env& e, Stack& s){
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

  return Lisp_ptr{new Function(code, arg_info)};
}

Lisp_ptr eval_if(const Cons* rest, Env& e, Stack& s){
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
  auto test_evaled = eval(test, e, s);
  if(!test_evaled){
    return {};
  }else if(test_evaled.get<bool>()){
    return eval(conseq, e, s);
  }else{
    return alt ? eval(alt, e, s) : Lisp_ptr{};
  }
}

Lisp_ptr eval_set(const Cons* rest, Env& e, Stack& s){
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

  auto val = valp->car();

  // evaluating
  if(s.find(var)){
    s.set(var, eval(val, e, s));
  }else if(e.find(var)){
    e.set(var, eval(val, e, s));
  }else{
    fprintf(stderr, "eval error: set! value is not defined previously!\n");
  }
      
  return {};
}

Lisp_ptr eval_define(const Cons* rest, Env& e, Stack& s){
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

    value = eval(val_l->car(), e, s);
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
    value = Lisp_ptr(func_value.get());
  }
    break;

  default:
    fprintf(stderr, "eval error: informal define syntax!\n");
    return {};
  }

  // assignment
  if(s.size() == 0){
    e.set(var, value);
  }else if(s.find(var)){
    s.set(var, value);
  }else{
    s.push(var, value);
  }

  func_value.release();
  return value;
}

} // namespace

Lisp_ptr eval(Lisp_ptr p, Env& e, Stack& s){
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

    if(auto rets = s.find(sym)){
      return rets;
    }else if(auto rete = e.find(sym)){
      return rete;
    }else{
      return Lisp_ptr{};
    }
  }
    
  case Ptr_tag::cons: {
    auto c = p.get<Cons*>();
    auto first = c->car();

    // special operator?
    if(first.tag() == Ptr_tag::symbol){
      auto sym = first.get<Symbol*>();
      auto k = to_keyword(sym->name().c_str());

      if(k != Keyword::not_keyword){
        if(c->cdr().tag() != Ptr_tag::cons){
          fprintf(stderr, "eval error: expresssion (<KEYWORD> . #) is informal!\n");
          return {};
        }

        Cons* r = c->cdr().get<Cons*>();
        if(!r){
          fprintf(stderr, "eval error: expresssion (<KEYWORD>) is informal!\n");
          return {};
        }

        switch(k){
        case Keyword::quote:  return r->car();
        case Keyword::lambda: return eval_lambda(r, e, s);
        case Keyword::if_:    return eval_if(r, e, s);
        case Keyword::set_:   return eval_set(r, e, s);

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

        case Keyword::define: return eval_define(r, e, s);

        case Keyword::else_:
        case Keyword::r_arrow:
        case Keyword::unquote:
        case Keyword::unquote_splicing:
          fprintf(stderr, "eval error: keyword '%s' cannot be used as operator!!\n",
                  stringify(k));
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
    auto proc = eval(first, e, s);
    if(proc.tag() != Ptr_tag::function){
      fprintf(stderr, "eval error: (# # ...)'s first element is not procedure!!\n");
      return {};
    }

    return funcall(proc.get<Function*>(), e, s, c->cdr());
  }
    
  default:
    UNEXP_DEFAULT();
  }
}
