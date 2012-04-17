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
  if(args != 2) return {};

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

