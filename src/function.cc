#include "function.hh"
#include "env.hh"
#include "stack.hh"
#include "eval.hh"
#include "cons.hh"

Lisp_ptr Function::call(Env& e, Stack& s){
  return {}; // stub
}

Lisp_ptr Function::call(Env& e, Stack& s, Lisp_ptr args){
  // push args
  int argc = 0;

  while(1){
    auto arg = args;
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
  auto ret = this->call(e, s);

  // pop args
  s.pop(argc);
  
  return ret;
}

