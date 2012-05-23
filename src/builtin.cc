#include "builtin.hh"
#include "number.hh"
#include "function.hh"
#include "lisp_ptr.hh"

using namespace std;

namespace {

Lisp_ptr plus_2(){
  Lisp_ptr p1 = VM.arg_get(0);
  if(p1.tag() != Ptr_tag::number){
    fprintf(stderr, "native func '+': first arg is not number!\n");
    return {};
  }
  Number* n1 = p1.get<Number*>();

  Lisp_ptr p2 = VM.arg_get(1);
  if(p2.tag() != Ptr_tag::number){
    fprintf(stderr, "native func '+': second arg is not number!\n");
    return {};
  }
  Number* n2 = p2.get<Number*>();

  Number* newn = new Number(n1->get<long>() + n2->get<long>());
  return Lisp_ptr(newn);
}




struct Entry {
  const char* name;
  Function func;
};

static Entry builtin_func[] = {
  {"+", Function{plus_2, {Lisp_ptr{}, 2, true}}}
};

} // namespace

void install_builtin(){
  for(auto& e : builtin_func){
    Symbol* s = VM.symtable.intern(e.name);
    VM.set(s, Lisp_ptr{&e.func});
  }
}

bool eq(Lisp_ptr a, Lisp_ptr b){
  if(a.tag() != b.tag()) return false;

  switch(a.tag()){
  case Ptr_tag::boolean:
    return a.get<bool>() == b.get<bool>();
  case Ptr_tag::character:
    return a.get<char>() == b.get<char>();
  default:
    return a.get<void*>() == b.get<void*>();
  }
}

bool eql(Lisp_ptr a, Lisp_ptr b){
  if(eq(a, b)) return true;

  if(a.tag() == Ptr_tag::number && b.tag() == Ptr_tag::number){
    return eql(*a.get<Number*>(), *b.get<Number*>());
  }

  return false;
}
