#include "lisp_ptr.hh"

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
  case Ptr_tag::function:
    return "function";
  case Ptr_tag::number:
    return "number";
  case Ptr_tag::string:
    return "string";
  case Ptr_tag::vector:
    return "vector";
  case Ptr_tag::port:
    return "port";
  default:
    return "(unknown PTR type)";
  }
}

void describe(FILE* f, Lisp_ptr p){
  fprintf(f, "[%s] %p", stringify(p.tag()), p.get<void*>());
}

