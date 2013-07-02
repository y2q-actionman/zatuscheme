#include <cassert>
#include <ostream>

#include "cons.hh"
#include "cons_util.hh"
#include "eval.hh"
#include "lisp_ptr.hh"
#include "printer.hh"
#include "procedure.hh"
#include "rational.hh"
#include "symbol.hh"
#include "s_closure.hh"
#include "token.hh"
#include "vm.hh"
#include "zs_error.hh"

using namespace std;

namespace {

void print_binary(ostream& f, int i){
  std::string tmp;

  while(i > 0){
    auto b = i % 2;
    tmp.push_back(b ? '1' : '0');
    i /= 2;
  }

  std::copy(tmp.rbegin(), tmp.rend(), ostreambuf_iterator<char>(f));
}

void print_integer(ostream& f, int i, int radix){
  if(radix == 10){
    f << i;
  }else{
    auto is_minus = (i < 0);
    auto u = std::abs(i);

    switch(radix){
    case 8:
      f << "#o";
      if(is_minus) f << '-';
      f << oct << u << dec;
      break;
    case 16:
      f << "#x";
      if(is_minus) f << '-';
      f << hex << u << dec;
      break;
    case 2:
      f << "#b";
      if(is_minus) f << '-';
      print_binary(f, u);
      break;
    default:
      UNEXP_DEFAULT();
    }
  }
}

void print_vector(ostream& f, const Vector* v, PrintReadable flag){
  auto i = v->begin();
  const auto e = v->end();

  f.write("#(", 2);

  while(i != e){
    print(f, *i, flag);
    ++i;
    if(i != e)
      f.put(' ');
  }

  f.put(')');
}

void print_list(ostream& f, Lisp_ptr l, PrintReadable flag){
  assert(l.tag() == Ptr_tag::cons);

  f.put('(');

  auto i = begin(l);
  for(; i; ++i){
    print(f, *i, flag);
    if(next(i)) f.put(' ');
  }

  if(!nullp(i.base())){
    f.write(" . ", 3);
    print(f, i.base(), flag);
  }

  f.put(')');
}

void print_char(ostream& f, char c, PrintReadable flag){
  if(flag == PrintReadable::t){
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
  
void print_string(ostream& f, const char* str, PrintReadable flag){
  if(flag == PrintReadable::t){
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

void print(ostream& f, Lisp_ptr p, PrintReadable flag, int radix){
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
    print_list(f, p, flag);
    break;

  case Ptr_tag::symbol: {
    auto sym = p.get<Symbol*>();
    auto interned = vm.symtable->find(sym->name()) != vm.symtable->end();
    if(!interned){
      f << "#<uninterned '";
    }
    f << sym->name();
    if(!interned){
      f << "' " << reinterpret_cast<void*>(sym) << ">";
    }
    break;
  }

  case Ptr_tag::integer:
    print_integer(f, p.get<int>(), radix);
    break;

  case Ptr_tag::rational: {
    auto r = p.get<Rational*>();
    f << r->numerator() << '/' << r->denominator();
    break;
  }

  case Ptr_tag::real:
    f << *p.get<double*>();
    break;

  case Ptr_tag::complex: {
    auto z = p.get<Complex*>();
    f << z->real() << showpos << z->imag() << noshowpos;
  }
    break;

  case Ptr_tag::string:
    print_string(f, p.get<String*>()->c_str(), flag);
    break;

  case Ptr_tag::vector:
    print_vector(f, p.get<Vector*>(), flag);
    break;

  case Ptr_tag::syntactic_closure: {
    auto sc = p.get<SyntacticClosure*>();
    if(flag == PrintReadable::t){
      f << "#<sc [";
    }
    print(f, sc->expr(), flag);
    if(flag == PrintReadable::t){
      f << "]>";
    }
    break;
  }

  case Ptr_tag::vm_argcount:
    f << "#<argcount " << p.get<int>() << ">";
    break;

  case Ptr_tag::vm_op:
    f << "#<VMop " << stringify(p.get<VMop>()) << ">";
    break;

  case Ptr_tag::n_procedure:
  case Ptr_tag::i_procedure:
  case Ptr_tag::continuation:
  case Ptr_tag::syntax_rules:
    f << "#<procedure [" << get_procname(p) << "]>";
    break;

  case Ptr_tag::input_port:
  case Ptr_tag::output_port:
  case Ptr_tag::env:
    f << "#<" << stringify(p.tag()) << " " << p.get<void*>() << ">";
    break;

  case Ptr_tag::notation:
    f << "#<" << stringify(p.tag()) << " " << stringify(p.get<Notation>()) << ">";
    
  default:
    f << "#<UNKNOWN TYPE " << static_cast<int>(p.tag()) << " " << p.get<void*>() << ">";
    break;
  }
}

std::ostream& operator<<(std::ostream& o, Lisp_ptr p){
  print(o, p, PrintReadable::t);
  return o;
}
