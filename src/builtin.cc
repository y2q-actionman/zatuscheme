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
  if(args != 2) return {};

  // Lisp_ptr lp = s.end() - 2;
  // if(lp.tag() != Ptr_tag::long_ptr
  //    || lp.get<Long_ptr*>()->tag() != Ptr_tag::number){
  //   return {};
  // }
  // Number* l = lp.get<Long_ptr*>()->get<Number*>();

  // Lisp_ptr rp = s.end() - 1;
  // if(rp.tag() != Ptr_tag::long_ptr
  //    || rp.get<Long_ptr*>()->tag() != Ptr_tag::number){
  //   return {};
  // }
  // Number* r = rp.get<Long_ptr*>()->get<Number*>();

  // Number* newn = new Number(l->get<long>() + r->get<long>());
  // return Lisp_ptr(new Long_ptr(newn));
}




struct Entry {
  const char* name;
  Function func;
};

static const
array<Entry, 1>
builtin_func{{
  {"+", Function{plus_2, {true, false, 2}}}
}};

} // namespace

void install_builtin(Env& env, SymTable& sym_t){
  for(auto& e : builtin_func){
    Symbol* s = sym_t.intern(e.name);
    env.set(s, Lisp_ptr{new Long_ptr(const_cast<Function*>(&e.func))});
  }
}

