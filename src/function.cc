#include "function.hh"
#include "env.hh"
#include "stack.hh"
#include "eval.hh"
#include "cons.hh"
#include "util.hh"

Lisp_ptr Function::call(Env& e, Stack& s, int args){
  // length check
  if(variadic_){
    if(args >= required_args_){
      fprintf(stderr, "funcall error: argcount insufficient! (supplied %d, required %d)\n",
              args, required_args_);
      return {};
    }    
  }else{
    if(args != required_args_){
      fprintf(stderr, "funcall error: argcount mismatch! (supplied %d, required %d)\n",
              args, required_args_);
      return {};
    }
  }

  // real call
  switch(type_){
  case Type::interpreted:
    return {}; // stub
  case Type::native:
    return n_func_(e, s, args);
  default:
    UNEXP_DEFAULT();
  }
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
  auto ret = this->call(e, s, argc);

  // pop args
  s.pop(argc);
  
  return ret;
}

