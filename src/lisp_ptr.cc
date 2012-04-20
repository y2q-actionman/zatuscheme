#include "lisp_ptr.hh"

static_assert(sizeof(Lisp_ptr) >= sizeof(char)*2, "pointer cannot be filled by chars");

template<>
void* Lisp_ptr::get() const{
  return u_.ptr_;
}

void describe(FILE* f, Ptr_tag p){
  switch(p){
  case Ptr_tag::undefined:
    fprintf(f, "undefined"); break;
  case Ptr_tag::boolean:
    fprintf(f, "boolean"); break;
  case Ptr_tag::character:
    fprintf(f, "character"); break; 
  case Ptr_tag::cons:
    fprintf(f, "cons"); break;
  case Ptr_tag::symbol:
    fprintf(f, "symbol"); break;
  case Ptr_tag::function:
    fprintf(f, "function"); break;
  case Ptr_tag::number:
    fprintf(f, "number"); break;
  case Ptr_tag::string:
    fprintf(f, "string"); break;
  case Ptr_tag::vector:
    fprintf(f, "vector"); break;
  case Ptr_tag::port:
    fprintf(f, "port"); break;
  default:
    fprintf(f, "(unknown type!)"); break;
  }
}

void describe(FILE* f, Lisp_ptr p){
  fprintf(f, "[");
  describe(f, p.tag());
  fprintf(f, "] %p\n", p.get<void*>());
}

