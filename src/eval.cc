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
    Symbol* sym = p.get<Symbol*>();
    if(to_keyword(sym->name().c_str()) == Keyword::not_keyword){
      return s.find(sym) ? e.find(sym) : Lisp_ptr{};
    }else{
      fprintf(stderr, "eval error: symbol '%s' is keyword!!", sym->name().c_str());
      return {};
    }
  }
    
  case Ptr_tag::cons: {
    Cons* c = p.get<Cons*>();
    Lisp_ptr first = c->car();

    // special operator?
    if(first.tag() == Ptr_tag::symbol){
      Symbol* sym = first.get<Symbol*>();
      Keyword k = to_keyword(sym->name().c_str());

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
    Lisp_ptr proc = eval(first, e, s);
    if(proc.tag() != Ptr_tag::function){
      fprintf(stderr, "eval error: expr's first element is not procedure!!");
      return {};
    }

    // push args into stack
    // TODO: cleanup dismissed stack in error..
    while(1){
      Lisp_ptr arg = c->cdr();
      if(arg.tag() != Ptr_tag::cons){
        fprintf(stderr, "eval error: arg contains non-cons (dotted list?)");
        return {};
      }

      Cons* arg_cell = arg.get<Cons*>();
      if(!arg_cell) // reached nil
        break;

      Lisp_ptr evaled = eval(arg_cell->car(), e, s);
      if(!evaled){
        fprintf(stderr, "eval error: evaluating func's arg failed!!");
        return {};
      }
      s.push(nullptr, evaled);

      arg = arg_cell->cdr();
    }

    // call
    return proc.get<Function*>()->call(e, s);
  }
    
  default:
    UNEXP_DEFAULT();
  }
}
