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

using namespace std;

namespace {

zs_error cons_type_check_failed(const char* func_name, Lisp_ptr p){
  return zs_error("native func: %s: arg is not %s! (%s)\n",
                       func_name, stringify(Ptr_tag::cons), stringify(p.tag()));
}

template<typename Fun>
inline
Lisp_ptr cons_carcdr(const char* name, Fun&& fun){
  ZsArgs args{1};
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
  ZsArgs args{2};
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed(name, args[0]);
  }
  
  auto c = args[0].get<Cons*>();
  if(!c){
    throw zs_error("native func: %s: arg is null list!\n", name);
  }
  
  return fun(c, args[1]);
}

Cons* cons_list_tail_base(const char* name){
  ZsArgs args{2};
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed(name, args[0]);
  }
  
  auto num = args[1].get<Number*>();
  if(!num || num->type() != Number::Type::integer){
    throw zs_error("native func: %s: passed radix is not number (%s).\n",
                        name, stringify(args[1].tag()));
  }
  auto nth = num->get<Number::integer_type>();

  for(auto i = begin(args[0]), e = end(args[0]); i != e; ++i){
    if(nth <= 0){
      return i.base().get<Cons*>();
    }
    --nth;
  }

  throw zs_error("native func: %s: passed list is shorter than expected (%ld).\n",
                      name, num->get<Number::integer_type>());
}

} // namespace


Lisp_ptr type_check_pair(){
  ZsArgs args{1};
  return Lisp_ptr{(args[0].tag() == Ptr_tag::cons) && !nullp(args[0])};
}

Lisp_ptr cons_cons(){
  ZsArgs args{2};
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
  ZsArgs args{1};
  return Lisp_ptr{nullp(args[0])};
}

Lisp_ptr cons_listp(){
  ZsArgs args{1};
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
  ZsArgs args{1};
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed("list?", args[0]);
  }

  return {new Number(std::distance(begin(args[0]), end(args[0])))};
}

Lisp_ptr cons_append(){
  ZsArgs args;
  GrowList gl;

  for(auto i = 0u; i < args.size() - 1; ++i){
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
  ZsArgs args{1};
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
  return cons_list_tail_base("list-tail");
}

Lisp_ptr cons_list_ref(){
  auto c = cons_list_tail_base("list-ref");
  return c->car();
}
