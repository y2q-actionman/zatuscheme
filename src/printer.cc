#include "printer.hh"

#include <cstdlib>
#include "symbol.hh"
#include "cons.hh"

namespace {

void print(FILE* f, const Number* n);
void print(FILE* f, const Vector* n);
void print(FILE* f, const Cons* n);

} // namespace

void print(FILE* f, Lisp_ptr p){
  switch(p.tag()){
  case Ptr_tag::immediate: {
    auto c = p.get<char>();
    if(c == static_cast<char>(0xff)){ // boolean
      if(p.get<bool>()){
        fprintf(f, "#t");
      }else{
        fprintf(f, "#f");
      }
    }else{
      fprintf(f, "%c", c);
    }
    break;
  }

  case Ptr_tag::cons:
    print(f, p.get<Cons*>());
    break;

  case Ptr_tag::symbol:
    fprintf(f, "%s", p.get<Symbol*>()->name().c_str());
    break;

  case Ptr_tag::long_ptr: {
    Long_ptr* l = p.get<Long_ptr*>();

    switch(l->tag()){
    case Ptr_tag::function:
      fprintf(f, "<function %p>", static_cast<void*>(l->get<Function*>()));
      break;
    case Ptr_tag::number:
      print(f, l->get<Number*>());
      break;
    case Ptr_tag::string:
      fprintf(f, "\"%s\"", l->get<String*>()->c_str());
      break;
    case Ptr_tag::vector:
      print(f, l->get<Vector*>());
      break;
    case Ptr_tag::port:
      fprintf(f, "<port %p>", static_cast<void*>(l->get<Port*>()));
      break;
    default:
      fprintf(stderr, "unexpected case!! (%s, %d)\n",
              __FILE__, __LINE__);
      abort();
    }
  }
    break;

  default:
    fprintf(stderr, "unexpected case!! (%s, %d)\n",
            __FILE__, __LINE__);
    abort();
  }
}
