#include <array>
#include <iterator>
#include <vector>
#include <functional>
#include <algorithm>
#include <numeric>

#include "builtin.hh"
#include "util.hh"
#include "number.hh"
#include "procedure.hh"
#include "lisp_ptr.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "printer.hh"

using namespace std;
using namespace Procedure;

namespace {

template<typename Fun>
inline void number_pred(Fun&& fun){
  auto arg = pick_args_1();
  auto num = arg.get<Number*>();
  if(!num){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  VM.return_value = Lisp_ptr{fun(num)};
}

void complexp(){
  number_pred([](Number* n){
      return n->type() >= Number::Type::integer;
    });
}

void realp(){
  number_pred([](Number* n){
      return n->type() >= Number::Type::integer
        && n->type() <= Number::Type::real;
    });
}

void rationalp(){
  number_pred([](Number* n){
      return n->type() >= Number::Type::integer
        && n->type() < Number::Type::real;
    });
}

void integerp(){
  number_pred([](Number* n){
      return n->type() == Number::Type::integer;
    });
}

void exactp(){
  number_pred([](Number* n){
      return n->type() == Number::Type::integer;
    });
}

void inexactp(){
  number_pred([](Number* n){
      return n->type() == Number::Type::complex
        || n->type() == Number::Type::real;
    });
}

void number_type_check_failed(const char* func_name, Lisp_ptr p){
  fprintf(zs::err, "native func: %s: arg is not number! (%s)\n",
          func_name, stringify(p.tag()));
  VM.return_value = {};
}

struct complex_found{
  static constexpr const char* msg = "native func: number compare: complex cannot be ordinated\n";
  bool operator()(const Number::complex_type&, const Number::complex_type&) const{
    fprintf(zs::err, msg);
    return false;
  }
};

template<template <typename> class Fun,
         class ComplexComparator = complex_found>
struct number_comparator {
  inline bool operator()(const Number* n1, const Number* n2){
    if(n1->type() == Number::Type::integer && n2->type() == Number::Type::integer){
      static const Fun<Number::integer_type> fun;
      return fun(n1->get<Number::integer_type>(), n2->get<Number::integer_type>());
    }else if(n1->type() <= Number::Type::real && n2->type() <= Number::Type::real){
      static const Fun<Number::real_type> fun;
      return fun(n1->coerce<Number::real_type>(), n2->coerce<Number::real_type>());
    }else{
      static const ComplexComparator fun;
      return fun(n1->coerce<Number::complex_type>(), n2->coerce<Number::complex_type>());
    }
  }
};


template<typename Fun>
inline void number_compare(const char* name, Fun&& fun){
  std::vector<Lisp_ptr> args;
  stack_to_vector(VM.stack, args);

  auto i1 = begin(args);
  const auto e = end(args);

  auto n1 = i1->get<Number*>();
  if(!n1 || n1->type() < Number::Type::integer){
    number_type_check_failed(name, *i1);
    return;
  }
                              
  for(auto i2 = next(i1); i2 != e; i1 = i2, ++i2){
    auto n2 = i2->get<Number*>();
    if(!n2 || n2->type() < Number::Type::integer){
      number_type_check_failed(name, *i2);
      return;
    }

    if(!fun(n1, n2)){
      VM.return_value = Lisp_ptr{false};
      return;
    }

    n1 = n2;
  }

  VM.return_value = Lisp_ptr{true};
}

void number_equal(){
  number_compare("=", 
                 number_comparator<std::equal_to,
                                   std::equal_to<Number::complex_type> >());
}

void number_less(){
  number_compare("<",
                 number_comparator<std::less>()); 
}

void number_greater(){
  number_compare(">",
                 number_comparator<std::greater>());
}
  
void number_less_eq(){
  number_compare("<=",
                 number_comparator<std::less_equal>());
}
  
void number_greater_eq(){
  number_compare(">=",
                 number_comparator<std::greater_equal>());
}


void zerop(){
  number_pred([](Number* num) -> bool {
      switch(num->type()){
      case Number::Type::complex: {
        auto c = num->get<Number::complex_type>();
        return (c.real() == 0 && c.imag() == 0);
      }
      case Number::Type::real:
        return num->get<Number::real_type>() == 0;
      case Number::Type::integer:
        return num->get<Number::integer_type>() == 0;
      case Number::Type::uninitialized:
      default:
        UNEXP_DEFAULT();
      }
    });
}

template<template <typename> class Fun>
struct pos_neg_pred{
  inline bool operator()(Number* num){
    static constexpr Fun<Number::integer_type> fun;

    switch(num->type()){
    case Number::Type::complex:
      return false;
    case Number::Type::real:
      return fun(num->get<Number::real_type>(), 0);
    case Number::Type::integer:
      return fun(num->get<Number::integer_type>(), 0);
    case Number::Type::uninitialized:
    default:
      UNEXP_DEFAULT();
    }
  }
};

void positivep(){
  number_pred(pos_neg_pred<std::greater>());
}

void negativep(){
  number_pred(pos_neg_pred<std::less>());
}

template<template <typename> class Fun>
struct even_odd_pred{
  inline bool operator()(Number* num){
    static constexpr Fun<Number::integer_type> fun;

    switch(num->type()){
    case Number::Type::complex:
    case Number::Type::real:
      return false;
    case Number::Type::integer:
      return fun(num->get<Number::integer_type>() % 2, 0);
    case Number::Type::uninitialized:
    default:
      UNEXP_DEFAULT();
    }
  }
};

void oddp(){
  number_pred(even_odd_pred<std::not_equal_to>());
}

void evenp(){
  number_pred(even_odd_pred<std::equal_to>());
}


template<class Fun>
inline
void number_accumulate(const char* name, Number&& init, Fun&& fun){
  std::vector<Lisp_ptr> args;
  stack_to_vector(VM.stack, args);

  for(auto i = begin(args), e = end(args);
      i != e; ++i){
    auto n = i->get<Number*>();
    if(!n){
      number_type_check_failed(name, *i);
      return;
    }

    if(!fun(init, *n)){
      VM.return_value = {};
      return;
    }
  }

  VM.return_value = {new Number(init)};
}

void number_max(){
  number_accumulate("max", Number(),
                    [](Number& n1, const Number& n2) -> bool {
                      if(n1.type() == Number::Type::uninitialized){
                        n1 = n2;
                        return true;
                      }

                      if(n2.type() == Number::Type::uninitialized){
                        return true;
                      }

                      if(n1.type() == Number::Type::integer && n2.type() == Number::Type::integer){
                        if(n1.get<Number::integer_type>() < n2.get<Number::integer_type>())
                          n1 = n2;
                        return true;
                      }

                      if(n1.type() <= Number::Type::real && n2.type() <= Number::Type::real){
                        if(n1.coerce<Number::real_type>() < n2.coerce<Number::real_type>())
                          n1 = Number{n2.coerce<Number::real_type>()};
                        if(n1.type() != Number::Type::real)
                          n1 = Number{n1.coerce<Number::real_type>()};
                        return true;
                      }

                      fprintf(zs::err, complex_found::msg);
                      return false;
                    });
}

void number_min(){
  number_accumulate("min", Number(),
                    [](Number& n1, const Number& n2) -> bool {
                      if(n1.type() == Number::Type::uninitialized){
                        n1 = n2;
                        return true;
                      }

                      if(n2.type() == Number::Type::uninitialized){
                        return true;
                      }

                      if(n1.type() == Number::Type::integer && n2.type() == Number::Type::integer){
                        if(n1.get<Number::integer_type>() > n2.get<Number::integer_type>())
                          n1 = n2;
                        return true;
                      }

                      if(n1.type() <= Number::Type::real && n2.type() <= Number::Type::real){
                        if(n1.coerce<Number::real_type>() > n2.coerce<Number::real_type>())
                          n1 = Number{n2.coerce<Number::real_type>()};
                        if(n1.type() != Number::Type::real)
                          n1 = Number{n1.coerce<Number::real_type>()};
                        return true;
                      }

                      fprintf(zs::err, complex_found::msg);
                      return false;
                    });
}

void number_plus(){
  number_accumulate("+", Number(0l),
                    [](Number& n1, const Number& n2) -> bool {
                      if(n1.type() == Number::Type::uninitialized){
                        n1 = n2;
                        return true;
                      }

                      if(n2.type() == Number::Type::uninitialized){
                        return true;
                      }

                      // n1 type <= n2 type
                      if(n1.type() == Number::Type::integer && n2.type() == Number::Type::integer){
                        n1.get<Number::integer_type>() += n2.get<Number::integer_type>();
                        return true;
                      }

                      if(n1.type() == Number::Type::real && n2.type() <= Number::Type::real){
                        n1.get<Number::real_type>() += n2.coerce<Number::real_type>();
                        return true;
                      }

                      if(n1.type() == Number::Type::complex && n2.type() <= Number::Type::complex){
                        n1.get<Number::complex_type>() += n2.coerce<Number::complex_type>();
                        return true;
                      }

                      // n1 type > n2 type
                      if(n1.type() < Number::Type::real && n2.type() == Number::Type::real){
                        n1 = Number{n1.coerce<Number::real_type>() + n2.get<Number::real_type>()};
                        return true;
                      }

                      if(n1.type() < Number::Type::complex && n2.type() == Number::Type::complex){
                        n1 = Number{n1.coerce<Number::complex_type>() + n2.get<Number::complex_type>()};
                        return true;
                      }

                      // ???
                      fprintf(zs::err, "native func: +: failed at numeric conversion!\n");
                      return false;
                    });
}


constexpr struct Entry {
  const char* name;
  const NProcedure func;

  constexpr Entry(const char* n, const NProcedure& f)
    : name(n), func(f){}
} builtin_numeric[] = {
  {"complex?", {
      complexp,
      Calling::function, {1, false}}},
  {"real?", {
      realp,
      Calling::function, {1, false}}},
  {"rational?", {
      rationalp,
      Calling::function, {1, false}}},
  {"integer?", {
      integerp,
      Calling::function, {1, false}}},

  {"exact?", {
      exactp,
      Calling::function, {1, false}}},
  {"inexact?", {
      inexactp,
      Calling::function, {1, false}}},

  {"=", {
      number_equal,
      Calling::function, {2, true}}},
  {"<", {
      number_less,
      Calling::function, {2, true}}},
  {">", {
      number_greater,
      Calling::function, {2, true}}},
  {"<=", {
      number_less_eq,
      Calling::function, {2, true}}},
  {">=", {
      number_greater_eq,
      Calling::function, {2, true}}},

  {"zero?", {
      zerop,
      Calling::function, {1, false}}},
  {"positive?", {
      positivep,
      Calling::function, {1, false}}},
  {"negative?", {
      negativep,
      Calling::function, {1, false}}},
  {"odd?", {
      oddp,
      Calling::function, {1, false}}},
  {"even?", {
      evenp,
      Calling::function, {1, false}}},

  {"max", {
      number_max,
      Calling::function, {2, true}}},
  {"min", {
      number_min,
      Calling::function, {2, true}}},

  {"+", {
      number_plus,
      Calling::function, {0, true}}}
};

} //namespace

void install_builtin_numeric(){
  for(auto& e : builtin_numeric){
    VM.set(intern(VM.symtable, e.name), {&e.func});
  }
}
