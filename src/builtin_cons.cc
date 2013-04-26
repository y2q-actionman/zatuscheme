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

zs_error cons_type_check_failed(const char* func_name, Lisp_ptr p){
  return zs_error_arg1(func_name,
                       printf_string("arg is not %s!", stringify(Ptr_tag::cons)),
                       {p});
}

template<typename Fun>
inline
Lisp_ptr cons_carcdr(ZsArgs args, const char* name, Fun fun){
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed(name, args[0]);
  }

  if(nullp(args[0])){
    throw zs_error_arg1(name, "arg is null list!");
  }

  return fun(args[0].get<Cons*>());
}

template<typename Fun>
inline
Lisp_ptr cons_set_carcdr(ZsArgs args, const char* name, Fun fun){
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed(name, args[0]);
  }
  
  if(nullp(args[0])){
    throw zs_error_arg1(name, "arg is null list!");
  }

  return fun(args[0].get<Cons*>(), args[1]);
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
  return cons_carcdr(move(args), "car", &car);
}

Lisp_ptr cons_cdr(ZsArgs args){
  return cons_carcdr(move(args), "cdr", &cdr);
}


Lisp_ptr cons_set_car(ZsArgs args){
  return cons_set_carcdr(move(args), "set-car!", &rplaca);
}

Lisp_ptr cons_set_cdr(ZsArgs args){
  return cons_set_carcdr(move(args), "set-cdr!", &rplacd);
}


Lisp_ptr cons_nullp(ZsArgs args){
  return Lisp_ptr{nullp(args[0])};
}

Lisp_ptr cons_listp(ZsArgs args){
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed("list?", args[0]);
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

Lisp_ptr cons_list(ZsArgs args){
  return make_cons_list(begin(args), end(args));
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
      throw cons_type_check_failed("append", args[i]);
    }

    for(auto p : args[i]){
      gl.push(p);
    }
  }

  // last
  return gl.extract_with_tail(args[args.size() - 1]);
}

template <typename Func>
Lisp_ptr cons_mem_funcs(ZsArgs args, const char* name, Func fun){
  if(args[1].tag() != Ptr_tag::cons){
    throw cons_type_check_failed(name, args[1]);
  }

  for(auto i = begin(args[1]), e = end(args[1]); i != e; ++i){
    if(fun(args[0], *i))
      return i.base();
  }

  return Lisp_ptr{false};
}

Lisp_ptr cons_memq(ZsArgs args){
  return cons_mem_funcs(move(args), "memq", eq_internal);
}

Lisp_ptr cons_memv(ZsArgs args){
  return cons_mem_funcs(move(args), "memv", eqv_internal);
}

Lisp_ptr cons_member(ZsArgs args){
  return cons_mem_funcs(move(args), "member", equal_internal);
}

} // namespace builtin
