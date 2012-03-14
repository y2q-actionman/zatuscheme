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
      switch(to_keyword(sym->name().c_str())){
        // ...

      case Keyword::not_keyword:
        break;

      default:
        UNEXP_DEFAULT();
      }
    }

    // macro call?
    //  try to find macro-function from symbol
    //    found -> macro expansion
    //    not found -> goto function calling

    // procedure call?
    Lisp_ptr proc = eval(first, e, s);
    if(proc.tag() != Ptr_tag::function){
      fprintf(stderr, "eval error: expr's first element is not procedured!!");
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
