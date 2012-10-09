#include <cstdlib>
#include <cassert>

#include "printer.hh"
#include "symbol.hh"
#include "cons.hh"
#include "number.hh"
#include "util.hh"
#include "delay.hh"

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

  do_list(l,
          [f](Cons* cell) -> bool{
            print(f, cell->car());
            if(cell->cdr().get<Cons*>()) fputc(' ', f);
            return true;
          },
          [f](Lisp_ptr dot_cdr){
            if(!nullp(dot_cdr)){
              fputs(" . ", f);
              print(f, dot_cdr);
            }
          });

  fputc(')', f);
}

void print_char(FILE* f, char c, print_human_readable flag){
  if(flag == print_human_readable::t){
    fputc(c, f);
  }else{
    switch(c){
    case ' ':
      fputs("#\\space", f);
      break;
    case '\n':
      fputs("#\\newline", f);
      break;
    default:
      fprintf(f, "#\\%c", c);
    }
  }
}
  
void print_string(FILE* f, const char* str, print_human_readable flag){
  if(flag == print_human_readable::t){
    fputs(str, f);
  }else{
    fputc('\"', f);
    for(auto s = str; *s; ++s){
      switch(*s){
      case '"':
        fputs("\\\"", f);
        break;
      case '\\':
        fputs("\\\\", f);
        break;
      default:
        fputc(*s, f);
      }
    }
    fputc('\"', f);
  }
}  


} // namespace

void print(FILE* f, Lisp_ptr p, print_human_readable flag){
  switch(p.tag()){
  case Ptr_tag::undefined:
    fprintf(f, "#<undefined>");
    break;
    
  case Ptr_tag::boolean:
    fprintf(f, p.get<bool>() ? "#t" : "#f");
    break;

  case Ptr_tag::character:
    print_char(f, p.get<char>(), flag);
    break;

  case Ptr_tag::cons:
    print_list(f, p);
    break;

  case Ptr_tag::symbol:
    fprintf(f, "%s", p.get<Symbol*>()->name().c_str());
    break;

  case Ptr_tag::number:
    print(f, *p.get<Number*>());
    break;

  case Ptr_tag::string:
    print_string(f, p.get<String*>()->c_str(), flag);
    break;

  case Ptr_tag::vector:
    print_vector(f, p.get<Vector*>());
    break;

  case Ptr_tag::delay: {
    auto d = p.get<Delay*>();
    fprintf(f, "#<delay (%s) [",
            d->forced() ? "forced" : "delaying");
    print(f, d->get(), flag);
    fprintf(f, "]>");
    break;
  }

  case Ptr_tag::i_procedure:
  case Ptr_tag::n_procedure:
  case Ptr_tag::continuation:
  case Ptr_tag::port:
  case Ptr_tag::env:
  case Ptr_tag::vm_op:
    fprintf(f, "#<%s %p>", stringify(p.tag()), p.get<void*>());
    break;

  default:
    UNEXP_DEFAULT();
  }
}
