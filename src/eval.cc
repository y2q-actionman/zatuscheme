#include "eval.hh"
#include "util.hh"
#include "keyword.hh"
#include "env.hh"
#include "symbol.hh"
#include "cons.hh"

using namespace std;

Lisp_ptr eval(Lisp_ptr p, Env& e){
  switch(p.tag()){
  case Ptr_tag::immediate:
  case Ptr_tag::long_ptr:
    return p; // self-evaluating

  case Ptr_tag::symbol: {
    Symbol* sym = p.get<Symbol*>();
    if(to_keyword(sym->name().c_str()) == Keyword::not_keyword){
      return e.find(sym);
    }else{
      fprintf(stderr, "eval error: symbol '%s' is keyword!!", sym->name().c_str());
      return {};
    }
  }
    
  case Ptr_tag::cons: {
    Cons* c = p.get<Cons*>();
    Lisp_ptr first = c->car();

    if(first.tag() == Ptr_tag::symbol){
      Symbol* sym = first.get<Symbol*>();
      switch(to_keyword(sym->name().c_str())){
        // dispatch special syntax handlers..

        // ...

      case Keyword::not_keyword:
        // macro call?
        //  trying find macro-function from symbol
        //    found -> macro expansion
        //    not found -> goto function calling
        break;

      default:
        UNEXP_DEFAULT();
      }
    }

    // procedure call?
    Lisp_ptr proc = eval(first, e);
    
    // try funcall!!
      
    break;
  }
    
  default:
    UNEXP_DEFAULT();
  }
}
