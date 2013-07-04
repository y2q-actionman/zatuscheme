#include "builtin_cons.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

using namespace std;

namespace {

template<typename Fun>
inline
Lisp_ptr with_nonnull_cons(const Lisp_ptr& p, Fun fun){
  auto c = p.get<Cons*>();
  if(!c){
    throw_zs_error(p, "invalid list!");
  }

  return fun(c);
}

} // namespace

namespace builtin {

Lisp_ptr cons_pairp(ZsArgs args){
  return Lisp_ptr{(args[0].tag() == Ptr_tag::cons) && !nullp(args[0])};
}

Lisp_ptr cons_cons(ZsArgs args){
  return {zs_new<Cons>(args[0], args[1])};
}

Lisp_ptr cons_car(ZsArgs args){
  return with_nonnull_cons(args[0], [](Cons* c) { return car(c); });
}

Lisp_ptr cons_cdr(ZsArgs args){
  return with_nonnull_cons(args[0], [](Cons* c) { return cdr(c); });
}


Lisp_ptr cons_set_car(ZsArgs args){
  auto& val = args[1];
  return with_nonnull_cons(args[0],
                           [&](Cons* c){ return rplaca(c, val); });
}

Lisp_ptr cons_set_cdr(ZsArgs args){
  auto& val = args[1];
  return with_nonnull_cons(args[0],
                           [&](Cons* c){ return rplacd(c, val); });
}

} // namespace builtin
