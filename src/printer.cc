#include <cstdlib>

#include "printer.hh"
#include "symbol.hh"
#include "cons.hh"
#include "number.hh"
#include "util.hh"

namespace {

void print(FILE* f, const Number* n){
  switch(n->type()){
  case Number::Type::uninitialized:
    fprintf(f, "(uninitialied number)");
    break;
  case Number::Type::complex: {
    auto&& z = n->get<Number::complex_type>();
    fprintf(f, "%g+%gi", z.real(), z.imag());
  }
    break;
  case Number::Type::real:
    fprintf(f, "%g", n->get<Number::real_type>());
    break;
  case Number::Type::integer:
    fprintf(f, "%ld", n->get<Number::integer_type>());
    break;
  default:
    UNEXP_DEFAULT();
  }
}

void print(FILE* f, const Vector* v){
  auto i = v->begin();
  const auto e = v->end();

  fputs("#(", f);

  while(i != e){
    print(f, *i);
    ++i;
    if(i != e)
      fputc(' ', f);
  }

  fputc(')', f);
}

void print(FILE* f, const Cons* c){
  fputc('(', f);

  while(c){
    print(f, c->car());

    Lisp_ptr rest = c->cdr();

    if(rest.tag() == Ptr_tag::cons){
      c = rest.get<Cons*>();
      if(c) fputc(' ', f);
    }else{
      fputs(" . ", f);
      print(f, rest);
      break;
    }
  }

  fputc(')', f);
}

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
      fprintf(f, "#\\%c", c);
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
      UNEXP_DEFAULT();
    }
  }
    break;

  default:
    UNEXP_DEFAULT();
  }
}
