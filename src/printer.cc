#include <cstdlib>
#include <cassert>

#include "printer.hh"
#include "symbol.hh"
#include "cons.hh"
#include "number.hh"
#include "util.hh"

namespace {

void print_vector(FILE* f, const Vector* v){
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

void print_list(FILE* f, Lisp_ptr l){
  assert(l.tag() == Ptr_tag::cons);

  fputc('(', f);

#if 0
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
#else
  do_list(l,
          [f](Lisp_ptr car, Lisp_ptr cdr) -> bool{
            print(f, car);
            if(cdr.get<Cons*>()) fputc(' ', f);
            return true;
          },
          [f](Lisp_ptr dot_cdr){
            fputs(" . ", f);
            print(f, dot_cdr);
          });
#endif

  fputc(')', f);
}

} // namespace

void print(FILE* f, Lisp_ptr p){
  switch(p.tag()){
  case Ptr_tag::undefined:
    fprintf(f, "<undefined>");
    break;
    
  case Ptr_tag::boolean:
    fprintf(f, p.get<bool>() ? "#t" : "#f");
    break;

  case Ptr_tag::character:
    fprintf(f, "#\\%c", p.get<char>());
    break;

  case Ptr_tag::cons:
    print_list(f, p);
    break;

  case Ptr_tag::symbol:
    fprintf(f, "%s", p.get<Symbol*>()->name().c_str());
    break;

  case Ptr_tag::function:
    fprintf(f, "<function %p>", static_cast<void*>(p.get<Function*>()));
    break;

  case Ptr_tag::number:
    print(f, *p.get<Number*>());
    break;

  case Ptr_tag::string:
    fprintf(f, "\"%s\"", p.get<String*>()->c_str());
    break;

  case Ptr_tag::vector:
    print_vector(f, p.get<Vector*>());
    break;

  case Ptr_tag::port:
    fprintf(f, "<port %p>", static_cast<void*>(p.get<Port*>()));
    break;

  default:
    UNEXP_DEFAULT();
  }
}
