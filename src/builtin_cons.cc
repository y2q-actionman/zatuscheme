#include <unordered_set>
#include <iterator>

#include "builtin_cons.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "number.hh"
#include "printer.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

namespace {

zs_error cons_type_check_failed(const char* func_name, Lisp_ptr p){
  return make_zs_error("native func: %s: arg is not %s! (%s)\n",
                       func_name, stringify(Ptr_tag::cons), stringify(p.tag()));
}

Lisp_ptr type_check_pair(){
  auto arg = pick_args_1();
  return Lisp_ptr{(arg.tag() == Ptr_tag::cons) && !nullp(arg)};
}

Lisp_ptr cons_cons(){
  auto args = pick_args<2>();
  return {new Cons(args[0], args[1])};
}


template<typename Fun>
inline
Lisp_ptr cons_carcdr(const char* name, Fun&& fun){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::cons){
    throw cons_type_check_failed(name, arg);
  }

  auto c = arg.get<Cons*>();
  if(!c){
    throw make_zs_error("native func: %s: arg is null list!\n", name);
  }
    
  return fun(c);
}

Lisp_ptr cons_car(){
  return cons_carcdr("car", [](Cons* c){ return c->car(); });
}

Lisp_ptr cons_cdr(){
  return cons_carcdr("cdr", [](Cons* c){ return c->cdr(); });
}


template<typename Fun>
inline
Lisp_ptr cons_set_carcdr(const char* name, Fun&& fun){
  auto args = pick_args<2>();
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed(name, args[0]);
  }
  
  auto c = args[0].get<Cons*>();
  if(!c){
    throw make_zs_error("native func: %s: arg is null list!\n", name);
  }
  
  return fun(c, args[1]);
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
  auto arg = pick_args_1();
  return Lisp_ptr{nullp(arg)};
}

Lisp_ptr cons_listp(){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::cons){
    throw cons_type_check_failed("list?", arg);
  }

  auto found_cons = unordered_set<Cons*>();
  
  auto ret = do_list(arg,
                     [&](Cons* c) -> bool{
                       auto founded = found_cons.find(c);
                       if(founded != found_cons.end())
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
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::cons){
    throw cons_type_check_failed("list?", arg);
  }

  return {new Number(std::distance(begin(arg), end(arg)))};
}

Lisp_ptr cons_append(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(vm.stack, args);

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
  return gl.extract_with_tail(args.back());
}

Lisp_ptr cons_reverse(){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::cons){
    throw cons_type_check_failed("reverse", arg);
  }

  Lisp_ptr ret = Cons::NIL;
  
  for(auto p : arg){
    ret = push_cons_list(p, ret);
  }

  return ret;
}

Cons* cons_list_tail_base(const char* name){
  auto args = pick_args<2>();
  if(args[0].tag() != Ptr_tag::cons){
    throw cons_type_check_failed(name, args[0]);
  }
  
  auto num = args[1].get<Number*>();
  if(!num || num->type() != Number::Type::integer){
    throw make_zs_error("native func: name: passed radix is not number (%s).\n",
                        stringify(args[1].tag()));
  }
  auto nth = num->get<Number::integer_type>();


  Cons* ret = nullptr;
  
  do_list(args[0],
          [&](Cons* c) -> bool{
            if(nth <= 0){
              ret = c;
              return false;
            }else{
              --nth;
              return true;
            }
          },
          [](Lisp_ptr){});

  return ret;
}

Lisp_ptr cons_list_tail(){
  auto c = cons_list_tail_base("list-tail");
  if(c){
    return {c};
  }else{
    return {};
  }
}

Lisp_ptr cons_list_ref(){
  auto c = cons_list_tail_base("list-ref");
  if(c){
    return c->car();
  }else{
    return {};
  }
}

} // namespace

const BuiltinFunc
builtin_cons[] = {
  {"pair?", {
      type_check_pair,
      {Calling::function, 1}}},

  {"cons", {
      cons_cons,
      {Calling::function, 2}}},

  {"car", {
      cons_car,
      {Calling::function, 1}}},
  {"cdr", {
      cons_cdr,
      {Calling::function, 1}}},

  {"set-car!", {
      cons_set_car,
      {Calling::function, 2}}},
  {"set-cdr!", {
      cons_set_cdr,
      {Calling::function, 2}}},

  {"null?", {
      cons_nullp,
      {Calling::function, 1}}},
  {"list?", {
      cons_listp,
      {Calling::function, 1}}},

  {"list", {
      cons_list,
      {Calling::function, 0, Variadic::t}}},
  {"list*", {
      cons_list_star,
      {Calling::function, 1, Variadic::t}}},

  {"length", {
      cons_length,
      {Calling::function, 1}}},

  {"append", {
      cons_append,
      {Calling::function, 1, Variadic::t}}},

  {"reverse", {
      cons_reverse,
      {Calling::function, 1}}},

  {"list-tail", {
      cons_list_tail,
      {Calling::function, 2}}},
  {"list-ref", {
      cons_list_ref,
      {Calling::function, 2}}},

};

const size_t builtin_cons_size = sizeof(builtin_cons) / sizeof(builtin_cons[0]);


const char* builtin_cons_load[] = {
  "(define (caar x) (car (car x)))",
  "(define (cadr x) (car (cdr x)))",
  "(define (cdar x) (cdr (car x)))",
  "(define (cddr x) (cdr (cdr x)))",

  "(define (caaar x) (car (caar x)))",
  "(define (caadr x) (car (cadr x)))",
  "(define (cadar x) (car (cdar x)))",
  "(define (caddr x) (car (cddr x)))",
  "(define (cdaar x) (cdr (caar x)))",
  "(define (cdadr x) (cdr (cadr x)))",
  "(define (cddar x) (cdr (cdar x)))",
  "(define (cdddr x) (cdr (cddr x)))",

  "(define (caaaar x) (car (caaar x)))",
  "(define (caaadr x) (car (caadr x)))",
  "(define (caadar x) (car (cadar x)))",
  "(define (caaddr x) (car (caddr x)))",
  "(define (cadaar x) (car (cdaar x)))",
  "(define (cadadr x) (car (cdadr x)))",
  "(define (caddar x) (car (cddar x)))",
  "(define (cadddr x) (car (cdddr x)))",
  "(define (cdaaar x) (cdr (caaar x)))",
  "(define (cdaadr x) (cdr (caadr x)))",
  "(define (cdadar x) (cdr (cadar x)))",
  "(define (cdaddr x) (cdr (caddr x)))",
  "(define (cddaar x) (cdr (cdaar x)))",
  "(define (cddadr x) (cdr (cdadr x)))",
  "(define (cdddar x) (cdr (cddar x)))",
  "(define (cddddr x) (cdr (cdddr x)))",

#define MEM_FUNCS(name, equal_op)               \
  "(define ("name" obj list)"                   \
  "  (if (null? list) #f"                       \
  "    (if ("equal_op" obj (car list)) list"    \
  "        ("name" obj (cdr list)))))"

  MEM_FUNCS("memq", "eq?"),
  MEM_FUNCS("memv", "eqv?"),
  MEM_FUNCS("member", "equal?"),
#undef MEM_FUNCS

#define ASS_FUNCS(name, equal_op)                       \
  "(define ("name" obj alist)"                          \
  "  (if (null? alist) #f"                              \
  "    (if ("equal_op" obj (caar alist)) (car alist)"   \
  "      ("name" obj (cdr alist)))))"

  ASS_FUNCS("assq", "eq?"),
  ASS_FUNCS("assv", "eqv?"),
  ASS_FUNCS("assoc", "equal?"),
#undef ASS_FUNCS

};

const size_t builtin_cons_load_size
= sizeof(builtin_cons_load) / sizeof(builtin_cons_load[0]);
