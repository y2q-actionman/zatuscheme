#include <unordered_set>

#include "builtin_cons.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "cons.hh"
#include "number.hh"
#include "printer.hh"

using namespace std;
using namespace Procedure;

namespace {

void cons_type_check_failed(const char* func_name, Lisp_ptr p){
  builtin_type_check_failed(func_name, Ptr_tag::cons, p);
}

void nil_check_failed(const char* func_name){
  fprintf(zs::err, "native func: %s: arg is null list!\n",
          func_name);
  vm.return_value[0] = {};
}

void type_check_pair(){
  auto arg = pick_args_1();
  vm.return_value[0] = Lisp_ptr{(arg.tag() == Ptr_tag::cons) && !nullp(arg)};
}

void cons_cons(){
  auto args = pick_args<2>();
  vm.return_value[0] = {new Cons(args[0], args[1])};
}


template<typename Fun>
inline
void cons_carcdr(const char* name, Fun&& fun){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::cons){
    cons_type_check_failed(name, arg);
    return;
  }

  auto c = arg.get<Cons*>();
  if(!c){
    nil_check_failed(name);
    return;
  }
    
  vm.return_value[0] = fun(c);
}

void cons_car(){
  cons_carcdr("car", [](Cons* c){ return c->car(); });
}

void cons_cdr(){
  cons_carcdr("cdr", [](Cons* c){ return c->cdr(); });
}


template<typename Fun>
inline
void cons_set_carcdr(const char* name, Fun&& fun){
  auto args = pick_args<2>();
  if(args[0].tag() != Ptr_tag::cons){
    cons_type_check_failed(name, args[0]);
    return;
  }
  
  auto c = args[0].get<Cons*>();
  if(!c){
    nil_check_failed(name);
    return;
  }

  
  vm.return_value[0] = fun(c, args[1]);
}

void cons_set_car(){
  cons_set_carcdr("set-car!",
                  [](Cons* c, Lisp_ptr p) -> Lisp_ptr {
                    c->rplaca(p);
                    return p;
                  });
}

void cons_set_cdr(){
  cons_set_carcdr("set-cdr!",
                  [](Cons* c, Lisp_ptr p) -> Lisp_ptr {
                    c->rplacd(p);
                    return p;
                  });
}


void cons_nullp(){
  auto arg = pick_args_1();
  vm.return_value[0] = Lisp_ptr{nullp(arg)};
}

void cons_listp(){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::cons){
    cons_type_check_failed("list?", arg);
    return;
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

  vm.return_value[0] = Lisp_ptr{ret};
}

void cons_list(){
  vm.return_value[0] = stack_to_list<false>(vm.stack);
}

void cons_list_star(){
  vm.return_value[0] = stack_to_list<true>(vm.stack);
}

void cons_length(){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::cons){
    cons_type_check_failed("list?", arg);
    return;
  }

  Number::integer_type length = 0;
  
  do_list(arg,
          [&](Cons*) -> bool{
            ++length;
            return true;
          },
          [](Lisp_ptr){});

  vm.return_value[0] = {new Number(length)};
}

void cons_append(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(vm.stack, args);

  Cons* head = new Cons;
  Cons* prev_c = head;
  Cons* now_c = head;

  for(auto i = 0u; i < args.size() - 1; ++i){
    if(args[i].tag() != Ptr_tag::cons){
      cons_type_check_failed("append", args[i]);
      return;
    }

    do_list(args[i],
            [&](Cons* c) -> bool{
              now_c->rplaca(c->car());

              auto new_c = new Cons;
              now_c->rplacd(new_c);

              prev_c = now_c;
              now_c = new_c;

              return true;
            },
            [](Lisp_ptr){});
  }

  // last
  if(prev_c == now_c){
    if(now_c == head){
      delete head;
      vm.return_value[0] = args.back();
    }else{
      head->rplacd(args.back());
      vm.return_value[0] = {head};
    }
  }else{
    prev_c->rplacd(args.back());
    delete now_c;
    vm.return_value[0] = {head};
  }
}

void cons_reverse(){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::cons){
    cons_type_check_failed("reverse", arg);
    return;
  }

  Lisp_ptr ret = Cons::NIL;
  
  do_list(arg,
          [&](Cons* c) -> bool{
            ret = {new Cons(c->car(), ret)};
            return true;
          },
          [](Lisp_ptr){});

  vm.return_value[0] = ret;
}

Cons* cons_list_tail_base(const char* name){
  auto args = pick_args<2>();
  if(args[0].tag() != Ptr_tag::cons){
    cons_type_check_failed(name, args[0]);
    return nullptr;
  }
  
  auto num = args[1].get<Number*>();
  if(!num){
    fprintf(zs::err, "native func: name: passed radix is not number (%s).\n",
            stringify(args[1].tag()));
    return nullptr;
  }
  if(num->type() != Number::Type::integer){
    fprintf(zs::err, "native func: name: passed radix is not number (%s).\n",
            stringify(args[1].tag()));
    return nullptr;
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

void cons_list_tail(){
  auto c = cons_list_tail_base("list-tail");
  if(c){
    vm.return_value[0] = {c};
  }else{
    vm.return_value[0] = {};
  }
}

void cons_list_ref(){
  auto c = cons_list_tail_base("list-ref");
  if(c){
    vm.return_value[0] = c->car();
  }else{
    vm.return_value[0] = {};
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


const char* builtin_cons_interpreted[] = {
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

  "(define (memq obj list)"
  "  (if (null? list) #f (if (eq? obj (car list)) list (memq obj (cdr list)))))",

  "(define (memv obj list)"
  "  (if (null? list) #f (if (eqv? obj (car list)) list (memv obj (cdr list)))))",

  "(define (member obj list)"
  "  (if (null? list) #f (if (equal? obj (car list)) list (member obj (cdr list)))))",

};

const size_t builtin_cons_interpreted_size
= sizeof(builtin_cons_interpreted) / sizeof(builtin_cons_interpreted[0]);
