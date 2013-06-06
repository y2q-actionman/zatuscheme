#include <unordered_set>
#include <iterator>

#include "builtin_cons.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "printer.hh"
#include "zs_error.hh"
#include "equality.hh"
#include "zs_memory.hh"

using namespace std;

namespace {

template<typename Fun>
inline
Lisp_ptr with_nonnull_cons(Lisp_ptr p, Fun fun){
  if(p.tag() != Ptr_tag::cons){
    throw builtin_type_check_failed(nullptr, Ptr_tag::cons, p);
  }

  if(nullp(p)){
    throw zs_error_arg1(nullptr, "arg is null list!");
  }

  return fun(p.get<Cons*>());
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
  return with_nonnull_cons(args[0], &car);
}

Lisp_ptr cons_cdr(ZsArgs args){
  return with_nonnull_cons(args[0], &cdr);
}


Lisp_ptr cons_set_car(ZsArgs args){
  auto val = args[1];
  return with_nonnull_cons(args[0],
                           [val](Cons* c){ return rplaca(c, val); });
}

Lisp_ptr cons_set_cdr(ZsArgs args){
  auto val = args[1];
  return with_nonnull_cons(args[0],
                           [val](Cons* c){ return rplacd(c, val); });
}


Lisp_ptr cons_listp(ZsArgs args){
  if(args[0].tag() != Ptr_tag::cons){
    throw builtin_type_check_failed(nullptr, Ptr_tag::cons, args[0]);
  }

  auto found_cons = unordered_set<Cons*>();

  auto i = begin(args[0]);
  for(; i; ++i){
    auto c = i.base().get<Cons*>();
    if(found_cons.find(c) != found_cons.end())
      return Lisp_ptr{false};   // circular list
    found_cons.insert(c);
  }

  if(!nullp(i.base())){
    return Lisp_ptr{false};     // dotted list
  }
  
  return Lisp_ptr{true};
}

Lisp_ptr cons_list_star(ZsArgs args){
  GrowList gl;

  for(auto i = 0; i < args.size() - 1; ++i){
    gl.push(args[i]);
  }

  return gl.extract_with_tail(args[args.size() - 1]);
}

Lisp_ptr cons_append(ZsArgs args){
  GrowList gl;

  for(auto i = 0; i < args.size() - 1; ++i){
    if(args[i].tag() != Ptr_tag::cons){
      throw builtin_type_check_failed(nullptr, Ptr_tag::cons, args[i]);
    }

    for(auto p : args[i]){
      gl.push(p);
    }
  }

  // last
  return gl.extract_with_tail(args[args.size() - 1]);
}

} // namespace builtin
