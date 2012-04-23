#include "builtin.hh"
#include "env.hh"
#include "stack.hh"
#include "number.hh"
#include "function.hh"
#include "lisp_ptr.hh"
#include "symtable.hh"

#include <array>

using namespace std;

namespace {

Lisp_ptr plus_2(Env& e, Stack& s, int args){
  (void)e;

  Lisp_ptr p1 = s.at(-args);
  if(p1.tag() != Ptr_tag::number){
    return {};
  }
  Number* n1 = p1.get<Number*>();

  Lisp_ptr p2 = s.at(-args+1);
  if(p2.tag() != Ptr_tag::number){
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

static
array<Entry, 1>
builtin_func{{
  {"+", Function{plus_2, {Lisp_ptr{}, 2, true}}}
}};

} // namespace

void install_builtin(Env& env, SymTable& sym_t){
  for(auto& e : builtin_func){
    Symbol* s = sym_t.intern(e.name);
    env.set(s, Lisp_ptr{&e.func});
  }
}

bool eq(Lisp_ptr a, Lisp_ptr b){
  return (a.tag() == b.tag())
    && (a.get<void*>() == b.get<void*>());
}

bool eql(Lisp_ptr a, Lisp_ptr b){
  if(eq(a, b)) return true;

  switch(a.tag()){
  case Ptr_tag::number:
    return (b.tag() == Ptr_tag::number)
      && eql(*a.get<Number*>(), *b.get<Number*>());
  case Ptr_tag::character:
    return (b.tag() == Ptr_tag::character)
      && a.get<char>() == b.get<char>();
  default:
    return false;
  }
}
