#include "builtin_cons.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"

using namespace std;
using namespace Procedure;

namespace {

void cons_type_check_failed(const char* func_name, Lisp_ptr p){
  fprintf(zs::err, "native func: %s: arg is not cons! (%s)\n",
          func_name, stringify(p.tag()));
  VM.return_value = {};
}

void nil_check_failed(const char* func_name){
  fprintf(zs::err, "native func: %s: arg is null list!\n",
          func_name);
  VM.return_value = {};
}

void type_check_pair(){
  auto arg = pick_args_1();
  VM.return_value = Lisp_ptr{(arg.tag() == Ptr_tag::cons) && !nullp(arg)};
}

void cons_cons(){
  auto args = pick_args<2>();
  VM.return_value = {new Cons(args[0], args[1])};
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
    
  VM.return_value = fun(c);
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

  
  VM.return_value = fun(c, args[1]);
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


constexpr BuiltinFunc
builtin_func[] = {
  {"pair?", {
      type_check_pair,
      Calling::function, {1, false}}},

  {"cons", {
      cons_cons,
      Calling::function, {2, false}}},

  {"car", {
      cons_car,
      Calling::function, {1, false}}},
  {"cdr", {
      cons_cdr,
      Calling::function, {1, false}}},

  {"set-car!", {
      cons_set_car,
      Calling::function, {2, false}}},
  {"set-cdr!", {
      cons_set_cdr,
      Calling::function, {2, false}}},
};

} // namespace

void install_builtin_cons(){
  for(auto& e : builtin_func){
    VM.set(intern(VM.symtable, e.name), {&e.func});
  }
}
