#include "decl.hh"
#include "zs_error.hh"

namespace zs {

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
  case Ptr_tag::integer:
    return "integer";
  case Ptr_tag::rational:
    return "rational";
  case Ptr_tag::real:
    return "real";
  case Ptr_tag::complex:
    return "complex";
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
  case Ptr_tag::syntactic_closure:
    return "syntactic closure";
  case Ptr_tag::syntax_rules:
    return "syntax rules";
  case Ptr_tag::vm_op:
    return "VMop";
  case Ptr_tag::vm_argcount:
    return "VMop (argcount)";
  case Ptr_tag::notation:
    return "notation";
  default:
    return "(unknown PTR type)";
  }
}

} // namespace zs
