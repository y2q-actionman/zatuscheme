#include "eval.hh"
#include "util.hh"
#include "keyword.hh"
#include "env.hh"
#include "symbol.hh"
#include "cons.hh"
#include "env.hh"
#include "stack.hh"
#include "function.hh"

using namespace std;

namespace {

Function::ArgInfo parse_func_arg(Lisp_ptr args){
  int argc = 0;
  Lisp_ptr p = args;

  while(1){
    if(p.tag() != Ptr_tag::cons){
      if(p.tag() != Ptr_tag::symbol){
        fprintf(stderr, "eval error: informal lambda list! (ended with non-symbol)\n");
        return {};
      }

      return {args, argc, true};
    }

    Cons* c = p.get<Cons*>();
    if(!c){
      return {args, argc, false};
    }

    if(c->car().tag() != Ptr_tag::symbol){
      fprintf(stderr, "eval error: informal lambda list! (includes non-symbol)\n");
      return {};
    }

    ++argc;
    p = c->cdr();
  }
}

Lisp_ptr funcall(const Function* fun, Env& e, Stack& s, Lisp_ptr args){
  const auto& argi = fun->arg_info();
  Lisp_ptr ret = {};

  // push args
  int argc = 0;
  auto arg = args;

  while(1){
    if(arg.tag() != Ptr_tag::cons){
      fprintf(stderr, "eval error: arg contains non-cons (dotted list?)\n");
      goto end;
    }

    auto arg_cell = arg.get<Cons*>();

    if(argc >= argi.required_args){
      if(argi.variadic){
        s.push(nullptr, arg);
        ++argc;
      }else{
        if(arg_cell){
          fprintf(stderr, "funcall error: argcount mismatch! (more than required %d)\n",
                  argi.required_args);
          goto end;
        }
      }
      break;
    }

    if(!arg_cell){ // reached nil
      fprintf(stderr, "funcall error: argcount mismatch! (less than required %d)\n",
              argi.required_args);
      goto end;
    }

    auto evaled = eval(arg_cell->car(), e, s);
    if(!evaled){
      fprintf(stderr, "eval error: evaluating func's arg failed!!\n");
      goto end;
    }

    s.push(nullptr, evaled);
    arg = arg_cell->cdr();
    ++argc;
  }

  // real call
  switch(fun->type()){
  case Function::Type::interpreted:
    ret = Lisp_ptr{}; // stub
    break;
  case Function::Type::native:
    ret = (fun->get<Function::NativeFunc>())(e, s, argc);
    break;
  default:
    UNEXP_DEFAULT();
  }

  // pop args
 end:
  s.pop(argc);
  
  return ret;
}

Lisp_ptr eval_special(Keyword k, const Cons* rest,
                      Env& e, Stack& s){
  switch(k){
  case Keyword::quote:
    return rest->car();

  case Keyword::lambda: {
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

  case Keyword::if_: {
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

    if(rest->cdr().tag() != Ptr_tag::cons){
      fprintf(stderr, "eval error: if has invalid alt!\n");
      return {};
    }

    auto alt = Lisp_ptr{};

    if(auto alt_l = rest->cdr().get<Cons*>()){
      alt = alt_l->car();

      if(rest->cdr().tag() != Ptr_tag::cons
         || rest->cdr().get<Cons*>()){
        fprintf(stderr, "eval error: if has extra alts!\n");
        return {};
      }
    }

    // evaluating
    auto test_evaled = eval(test, e, s);
    if(test_evaled.get<bool>()){
      return eval(conseq, e, s);
    }else{
      return alt ? eval(alt, e, s) : Lisp_ptr{};
    }
  }

  case Keyword::set_: {
    // extracting
    auto var = rest->car().get<Symbol*>();
    if(!var){
      fprintf(stderr, "eval error: set!'s first element is not symbol!\n");
      return {};
    }
    if(to_keyword(var->name()) != Keyword::not_keyword){
      fprintf(stderr, "eval error: set!'s first element is Keyword!\n");
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

  case Keyword::cond:
    // now implementing..
    return {};

  case Keyword::case_:
    // now implementing..
    return {};

  case Keyword::and_:
    // now implementing..
    return {};

  case Keyword::or_:
    // now implementing..
    return {};

  case Keyword::let:
    // now implementing..
    return {};

  case Keyword::let_star:
    // now implementing..
    return {};

  case Keyword::letrec:
    // now implementing..
    return {};

  case Keyword::begin:
    // now implementing..
    return {};

  case Keyword::do_:
    // now implementing..
    return {};

  case Keyword::delay:
    // now implementing..
    return {};

  case Keyword::quasiquote:
    // now implementing..
    return {};

  case Keyword::else_:
  case Keyword::r_arrow:
  case Keyword::unquote:
  case Keyword::unquote_splicing:
    fprintf(stderr, "eval error: keyword '%s' cannot be used as operator!!\n",
            stringify(k));
    return {};
    
  case Keyword::define:
    fprintf(stderr, "eval error: definition cannot be treated in eval!!\n");
    return {};

  case Keyword::not_keyword:
    fprintf(stderr, "internal error: should not be procesed normal symbols here!!\n");
  default:
    UNEXP_DEFAULT();
  }
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
    if(to_keyword(sym->name()) != Keyword::not_keyword){
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
      auto k = to_keyword(sym->name());

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
        return eval_special(k, r, e, s);
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
