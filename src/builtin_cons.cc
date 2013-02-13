#include <unordered_set>
#include <iterator>

#include "builtin_cons.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "number.hh"
#include "printer.hh"
#include "zs_error.hh"
#include "equality.hh"

using namespace std;

namespace {

zs_error cons_type_check_failed(const char* func_name, Lisp_ptr p){
  return zs_error("native func: %s: arg is not %s! (%s)\n",
                       func_name, stringify(Ptr_tag::cons), stringify(p.tag()));
}

template<typename Fun>
inline
Lisp_ptr cons_carcdr(const char* name, Fun&& fun){
  ZsArgs args;
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed(name, args[0]);
  }

  auto c = args[0].get<Cons*>();
  if(!c){
    throw zs_error("native func: %s: arg is null list!\n", name);
  }
    
  return fun(c);
}

template<typename Fun>
inline
Lisp_ptr cons_set_carcdr(const char* name, Fun&& fun){
  ZsArgs args;
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed(name, args[0]);
  }
  
  auto c = args[0].get<Cons*>();
  if(!c){
    throw zs_error("native func: %s: arg is null list!\n", name);
  }
  
  return fun(c, args[1]);
}

} // namespace


Lisp_ptr type_check_pair(){
  ZsArgs args;
  return Lisp_ptr{(args[0].tag() == Ptr_tag::cons) && !nullp(args[0])};
}

Lisp_ptr cons_cons(){
  ZsArgs args;
  return {new Cons(args[0], args[1])};
}

Lisp_ptr cons_car(){
  return cons_carcdr("car", [](Cons* c){ return c->car(); });
}

Lisp_ptr cons_cdr(){
  return cons_carcdr("cdr", [](Cons* c){ return c->cdr(); });
}


Lisp_ptr cons_set_car(){
  return cons_set_carcdr("set-car!",
                         [](Cons* c, Lisp_ptr p) -> Lisp_ptr {
                           c->rplaca(p);
                           return p;
                         });
}

Lisp_ptr cons_set_cdr(){
  return cons_set_carcdr("set-cdr!",
                         [](Cons* c, Lisp_ptr p) -> Lisp_ptr {
                           c->rplacd(p);
                           return p;
                         });
}


Lisp_ptr cons_nullp(){
  ZsArgs args;
  return Lisp_ptr{nullp(args[0])};
}

Lisp_ptr cons_listp(){
  ZsArgs args;
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed("list?", args[0]);
  }

  auto found_cons = unordered_set<Cons*>();
  
  auto ret = do_list(args[0],
                     [&](Cons* c) -> bool{
                       auto found = found_cons.find(c);
                       if(found != found_cons.end())
                         return false; //circular list
                       
                       found_cons.insert(c);
                       return true;
                     },
                     [](Lisp_ptr p){
                       return nullp(p);
                     });

  return Lisp_ptr{ret};
}

Lisp_ptr cons_list(){
  return stack_to_list<false>(vm.stack);
}

Lisp_ptr cons_list_star(){
  return stack_to_list<true>(vm.stack);
}

Lisp_ptr cons_length(){
  ZsArgs args;
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed("list?", args[0]);
  }

  return {new Number(std::distance(begin(args[0]), end(args[0])))};
}

Lisp_ptr cons_append(){
  ZsArgs args;
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

Lisp_ptr cons_reverse(){
  ZsArgs args;
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed("reverse", args[0]);
  }

  Lisp_ptr ret = Cons::NIL;
  
  for(auto p : args[0]){
    ret = push_cons_list(p, ret);
  }

  return ret;
}

Lisp_ptr cons_list_tail(){
  ZsArgs args;
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed("list-tail", args[0]);
  }
  
  auto num = args[1].get<Number*>();
  if(!num || num->type() != Number::Type::integer){
    throw zs_error("native func: list-tail: passed radix is not number (%s).\n",
                   stringify(args[1].tag()));
  }
  auto nth = num->get<Number::integer_type>();

  for(auto i = begin(args[0]), e = end(args[0]); i != e; ++i){
    if(nth <= 0){
      return i.base();
    }
    --nth;
  }

  throw zs_error("native func: list-tail: passed list is shorter than expected (%ld).\n",
                 num->get<Number::integer_type>());
}

template <typename Func>
Lisp_ptr cons_mem_funcs(const char* name, Func fun){
  ZsArgs args;

  if(args[1].tag() != Ptr_tag::cons){
    throw cons_type_check_failed(name, args[1]);
  }

  for(auto i = begin(args[1]), e = end(args[1]); i != e; ++i){
    if(fun(args[0], *i))
      return i.base();
  }

  return Lisp_ptr{false};
}

Lisp_ptr cons_memq(){
  return cons_mem_funcs("memq", eq_internal);
}

Lisp_ptr cons_memv(){
  return cons_mem_funcs("memv", eqv_internal);
}

Lisp_ptr cons_member(){
  return cons_mem_funcs("member", equal_internal);
}
