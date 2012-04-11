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

Function::ArgInfo parse_func_arg(Lisp_ptr args);

Lisp_ptr funcall(const Function* fun, Env& e, Stack& s, Lisp_ptr args){
  // push args
  int argc = 0;
  auto arg = args;

  while(1){
    if(arg.tag() != Ptr_tag::cons){
      fprintf(stderr, "eval error: arg contains non-cons (dotted list?)");
      return {};
    }

    auto arg_cell = arg.get<Cons*>();
    if(!arg_cell) // reached nil
      break;

    auto evaled = eval(arg_cell->car(), e, s);
    if(!evaled){
      fprintf(stderr, "eval error: evaluating func's arg failed!!");
      return {};
    }

    s.push(nullptr, evaled);
    arg = arg_cell->cdr();
    ++argc;
  }

  // real calling
  const auto& argi = fun->arg_info();

  // length check (TODO: merge this with pushing phase)
  if(argi.variadic){
    if(argc >= argi.required_args){
      fprintf(stderr, "funcall error: argcount insufficient! (supplied %d, required %d)\n",
              argc, argi.required_args);
      return {};
    }    
  }else{
    if(argc != argi.required_args){
      fprintf(stderr, "funcall error: argcount mismatch! (supplied %d, required %d)\n",
              argc, argi.required_args);
      return {};
    }
  }

  // real call
  Lisp_ptr ret;

  switch(fun->type()){
  case Function::Type::interpreted:
    ret = Lisp_ptr{}; // stub
  case Function::Type::native:
    ret = (fun->func<Function::NativeFunc>())(e, s, argc);
  default:
    UNEXP_DEFAULT();
  }

  // pop args
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
      fprintf(stderr, "eval error: lambda has invalid body!");
      return {};
    }

    auto arg_info = parse_func_arg(args);
    if(!arg_info.valid){
      fprintf(stderr, "eval error: lambda has invalid args!");
      return {};
    }

    auto code = rest->cdr();
    if(!code.get<Cons*>()){
      fprintf(stderr, "eval error: lambda has no body!");
      return {};
    }

    return Lisp_ptr{new Long_ptr{new Function(code, arg_info)}};
  }

  case Keyword::if_: {
    // extracting
    auto test = rest->car();

    if(rest->cdr().tag() != Ptr_tag::cons){
      fprintf(stderr, "eval error: if has invalid conseq!");
      return {};
    }

    auto conseq_l = rest->cdr().get<Cons*>();
    if(!conseq_l){
      fprintf(stderr, "eval error: if has no conseq!");
      return {};
    }

    auto conseq = conseq_l->car();

    if(rest->cdr().tag() != Ptr_tag::cons){
      fprintf(stderr, "eval error: if has invalid alt!");
      return {};
    }

    auto alt = Lisp_ptr{};

    if(auto alt_l = rest->cdr().get<Cons*>()){
      alt = alt_l->car();

      if(rest->cdr().tag() != Ptr_tag::cons
         || rest->cdr().get<Cons*>()){
        fprintf(stderr, "eval error: if has extra alts!");
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
      fprintf(stderr, "eval error: set!'s first element is not symbol!");
      return {};
    }
    if(to_keyword(var->name()) != Keyword::not_keyword){
      fprintf(stderr, "eval error: set!'s first element is Keyword!");
      return {};
    }

    if(rest->cdr().tag() != Ptr_tag::cons){
      fprintf(stderr, "eval error: set!'s value form is informal!");
      return {};
    }

    auto valp = rest->cdr().get<Cons*>();
    if(!valp){
      fprintf(stderr, "eval error: set! has no value!");
      return {};
    }

    if(valp->cdr().tag() != Ptr_tag::cons
       || valp->cdr().get<Cons*>()){
      fprintf(stderr, "eval error: set! has extra forms!");
      return {};
    }

    auto val = valp->car();

    // evaluating
    if(s.find(var)){
      s.set(var, eval(val, e, s));
    }else if(e.find(var)){
      e.set(var, eval(val, e, s));
    }else{
      fprintf(stderr, "eval error: set! value is not defined previously!");
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
    fprintf(stderr, "eval error: keyword '%s' cannot be used as operator!!",
            stringify(k));
    return {};
    
  case Keyword::define:
    fprintf(stderr, "eval error: definition cannot be treated in eval!!");
    return {};

  case Keyword::not_keyword:
    fprintf(stderr, "internal error: should not be procesed normal symbols here!!");
  default:
    UNEXP_DEFAULT();
  }
}

} // namespace

Lisp_ptr eval(Lisp_ptr p, Env& e, Stack& s){
  if(!p){
    fprintf(stderr, "eval error: undefined value passed!!");
    return {};
  }

  switch(p.tag()){
  case Ptr_tag::immediate:
  case Ptr_tag::long_ptr:
    return p; // self-evaluating

  case Ptr_tag::symbol: {
    auto sym = p.get<Symbol*>();
    if(to_keyword(sym->name()) == Keyword::not_keyword){
      return s.find(sym) ? e.find(sym) : Lisp_ptr{};
    }else{
      fprintf(stderr, "eval error: symbol '%s' is keyword!!", sym->name().c_str());
      return {};
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
          fprintf(stderr, "eval error: expresssion (<KEYWORD> . #) is informal!");
          return {};
        }

        Cons* r = c->cdr().get<Cons*>();
        if(!r){
          fprintf(stderr, "eval error: expresssion (<KEYWORD>) is informal!");
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
      fprintf(stderr, "eval error: (# # ...)'s first element is not procedure!!");
      return {};
    }

    return funcall(proc.get<Function*>(), e, s, c->cdr());
  }
    
  default:
    UNEXP_DEFAULT();
  }
}
