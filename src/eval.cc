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

Lisp_ptr eval_special(Keyword k, const Cons* rest,
                      Env& e, Stack& s){
  switch(k){
  case Keyword::quote:
    return rest->car();

  case Keyword::lambda:
    // now implementing..
    return {};

  case Keyword::if_:
    // now implementing..
    return {};

  case Keyword::set_:
    // now implementing..
    return {};

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
    fprintf(stderr, "eval error: definition cannot be treated in eval!!",
            stringify(k));
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
    if(to_keyword(sym->name().c_str()) == Keyword::not_keyword){
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
      auto k = to_keyword(sym->name().c_str());

      if(k != Keyword::not_keyword){
        if(c->cdr().tag() != Ptr_tag::cons){
          fprintf(stderr, "eval error: expresssion (<KEYWORD> . #) is informal!");
          return {};
        }
        return eval_special(k, c->cdr().get<Cons*>(), e, s);
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

    return proc.get<Function*>()->call(e, s, c->cdr());
  }
    
  default:
    UNEXP_DEFAULT();
  }
}
