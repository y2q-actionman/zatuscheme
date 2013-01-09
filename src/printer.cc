#include <cassert>
#include <ostream>

#include "lisp_ptr.hh"
#include "printer.hh"
#include "symbol.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "number.hh"
#include "util.hh"
#include "delay.hh"
#include "vm.hh"
#include "eval.hh"
#include "s_closure.hh"

using namespace std;

namespace {

void print_vector(ostream& f, const Vector* v){
  auto i = v->begin();
  const auto e = v->end();

  f.write("#(", 2);

  while(i != e){
    print(f, *i);
    ++i;
    if(i != e)
      f.put(' ');
  }

  f.put(')');
}

void print_list(ostream& f, Lisp_ptr l){
  assert(l.tag() == Ptr_tag::cons);

  f.put('(');

  do_list(l,
          [&f](Cons* cell) -> bool{
            print(f, cell->car());
            if(cell->cdr().get<Cons*>()) f.put(' ');
            return true;
          },
          [&f](Lisp_ptr dot_cdr){
            if(!nullp(dot_cdr)){
              f.write(" . ", 3);
              print(f, dot_cdr);
            }
          });

  f.put(')');
}

void print_char(ostream& f, char c, print_human_readable flag){
  if(flag == print_human_readable::t){
    f.put(c);
  }else{
    switch(c){
    case ' ':
      f << "#\\space";
      break;
    case '\n':
      f << "#\\newline";
      break;
    default:
      f.write("#\\", 2);
      f.put(c);
    }
  }
}
  
void print_string(ostream& f, const char* str, print_human_readable flag){
  if(flag == print_human_readable::t){
    f << str;
  }else{
    f.put('\"');
    for(auto s = str; *s; ++s){
      switch(*s){
      case '"':
        f.write("\\\"", 2);
        break;
      case '\\':
        f.write("\\\\", 2);
        break;
      default:
        f.put(*s);
      }
    }
    f.put('\"');
  }
}  


} // namespace

void print(ostream& f, Lisp_ptr p, print_human_readable flag){
  switch(p.tag()){
  case Ptr_tag::undefined:
    f << "#<undefined>";
    break;
    
  case Ptr_tag::boolean:
    f << ((p.get<bool>()) ? "#t" : "#f");
    break;

  case Ptr_tag::character:
    print_char(f, p.get<char>(), flag);
    break;

  case Ptr_tag::cons:
    print_list(f, p);
    break;

  case Ptr_tag::symbol: {
    auto sym = p.get<Symbol*>();
    if(vm.symtable().find(sym->name()) != vm.symtable().end()){
      f << sym->name();
    }else{
      f << "#<uninterned '" << sym->name() << "' " << c_cast<void*>(sym) << ">";
    }
    break;
  }

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
    if(flag == print_human_readable::t || !d->forced()){
      f << "#<delay (" << (d->forced() ? "forced" : "delayed") << ") [";
    }
    print(f, d->get(), flag);
    if(flag == print_human_readable::t || !d->forced()){
      f << "]>";
    }
    break;
  }

  case Ptr_tag::syntactic_closure: {
    auto sc = p.get<SyntacticClosure*>();
    f << "#<SyntacticClosure [";
    print(f, sc->expr(), flag);
    f << ']';
    if(identifierp(p))
      f << " (identifier)";
    f << '>';
    break;
  }

  case Ptr_tag::vm_argcount:
    f << "#<argcount " << p.get<int>() << ">";
    break;

  case Ptr_tag::vm_op:
    f << "#<VMop " << stringify(p.get<VMop>()) << ">";
    break;

  case Ptr_tag::i_procedure:
  case Ptr_tag::n_procedure:
  case Ptr_tag::continuation:
  case Ptr_tag::input_port:
  case Ptr_tag::output_port:
  case Ptr_tag::env:
  case Ptr_tag::syntax_rules:
    f << "#<" << stringify(p.tag()) << " " << p.get<void*>() << ">";
    break;

  default:
    UNEXP_DEFAULT();
  }
}
