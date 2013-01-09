#include "lisp_ptr.hh"
#include "zs_error.hh"

bool operator==(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() != b.tag()) return false;

  switch(a.tag()){
  case Ptr_tag::undefined:
    return true;
  case Ptr_tag::boolean:
    return a.get<bool>() == b.get<bool>();
  case Ptr_tag::character:
     // this can be moved into eqv? in R5RS, but char is contained in Lisp_ptr.
    return a.get<char>() == b.get<char>();
  case Ptr_tag::cons:
  case Ptr_tag::symbol:
  case Ptr_tag::i_procedure:
  case Ptr_tag::n_procedure:
  case Ptr_tag::continuation:
  case Ptr_tag::number:
  case Ptr_tag::string:
  case Ptr_tag::vector:
  case Ptr_tag::input_port:
  case Ptr_tag::output_port:
  case Ptr_tag::env:
  case Ptr_tag::delay:
  case Ptr_tag::syntactic_closure:
  case Ptr_tag::syntax_rules:
  case Ptr_tag::vm_op:
    return a.get<void*>() == b.get<void*>();
  case Ptr_tag::vm_argcount:
    return a.get<int>() == b.get<int>();
  default:
    UNEXP_DEFAULT();
  }
}

const char* stringify(Ptr_tag p){
  switch(p){
  case Ptr_tag::undefined:
    return "undefined";
  case Ptr_tag::boolean:
    return "boolean";
  case Ptr_tag::character:
    return "character";
  case Ptr_tag::cons:
    return "cons";
  case Ptr_tag::symbol:
    return "symbol";
  case Ptr_tag::i_procedure:
    return "interpreted procedure";
  case Ptr_tag::n_procedure:
    return "native procedure";
  case Ptr_tag::continuation:
    return "continuation";
  case Ptr_tag::number:
    return "number";
  case Ptr_tag::string:
    return "string";
  case Ptr_tag::vector:
    return "vector";
  case Ptr_tag::input_port:
    return "input port";
  case Ptr_tag::output_port:
    return "output port";
  case Ptr_tag::env:
    return "env";
  case Ptr_tag::delay:
    return "delay";
  case Ptr_tag::syntactic_closure:
    return "syntactic closure";
  case Ptr_tag::syntax_rules:
    return "syntax rules";
  case Ptr_tag::vm_op:
    return "VMop";
  case Ptr_tag::vm_argcount:
    return "VMop (argcount)";
  default:
    return "(unknown PTR type)";
  }
}
