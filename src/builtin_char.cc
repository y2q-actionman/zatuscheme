#include <functional>
#include <cctype>

#include "builtin_char.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "number.hh"
#include "zs_error.hh"

using namespace std;

namespace {

zs_error char_type_check_failed(const char* func_name, Lisp_ptr p){
  return zs_error("native func: %s: arg is not %s! (%s)\n",
                       func_name, stringify(Ptr_tag::character), stringify(p.tag()));
}

template<typename Fun>
Lisp_ptr char_compare(const char* name, Fun&& fun){
  ZsArgs args;
  char c[2];

  for(auto i = 0; i < 2; ++i){
    c[i] = args[i].get<char>();
    if(!c[i]){
      throw char_type_check_failed(name, args[i]);
    }
  }

  return Lisp_ptr{fun(c[0], c[1])};
}

template<typename Fun>
struct ci_comparator{
  inline bool operator()(char c1, char c2) const{
    static constexpr Fun fun;
    return fun(tolower(c1), tolower(c2));
  }
};

template<typename Fun>
Lisp_ptr char_pred(Fun&& fun){
  ZsArgs args;

  auto c = args[0].get<char>();
  if(!c){
    return Lisp_ptr{false};
  }

  return Lisp_ptr{fun(c)};
}

template<typename Fun>
Lisp_ptr char_conversion(const char* name, Fun&& fun){
  ZsArgs args;

  auto c = args[0].get<char>();
  if(!c){
    throw char_type_check_failed(name, args[0]);
  }

  return Lisp_ptr{fun(c)};
}
  
} // namespace


Lisp_ptr char_eq(){
  return char_compare("char=?", std::equal_to<char>());
}

Lisp_ptr char_less(){
  return char_compare("char<?", std::less<char>());
}

Lisp_ptr char_greater(){
  return char_compare("char>?", std::greater<char>());
}

Lisp_ptr char_less_eq(){
  return char_compare("char<=?", std::less_equal<char>());
}

Lisp_ptr char_greater_eq(){
  return char_compare("char>=?", std::greater_equal<char>());
}
  

Lisp_ptr char_ci_eq(){
  return char_compare("char-ci=?", ci_comparator<std::equal_to<int> >());
}

Lisp_ptr char_ci_less(){
  return char_compare("char-ci<?", ci_comparator<std::less<int> >());
}

Lisp_ptr char_ci_greater(){
  return char_compare("char-ci>?", ci_comparator<std::greater<int> >());
}

Lisp_ptr char_ci_less_eq(){
  return char_compare("char-ci<=?", ci_comparator<std::less_equal<int> >());
}

Lisp_ptr char_ci_greater_eq(){
  return char_compare("char-ci>=?", ci_comparator<std::greater_equal<int> >());
}

  
Lisp_ptr char_isalpha(){
  return char_pred([](char c) -> bool{ return std::isalpha(c); });
}

Lisp_ptr char_isdigit(){
  return char_pred([](char c) -> bool{ return std::isdigit(c); });
}

Lisp_ptr char_isspace(){
  return char_pred([](char c) -> bool{ return std::isspace(c); });
}

Lisp_ptr char_isupper(){
  return char_pred([](char c) -> bool{ return std::isupper(c); });
}

Lisp_ptr char_islower(){
  return char_pred([](char c) -> bool{ return std::islower(c); });
}


Lisp_ptr char_to_int(){
  return char_conversion("char->integer",
                         [](char c){
                           return new Number(static_cast<Number::integer_type>(c));
                         });
}

Lisp_ptr char_from_int(){
  ZsArgs args;

  auto n = args[0].get<Number*>();
  if(!n){
    throw builtin_type_check_failed("integer->char", Ptr_tag::number, args[0]);
  }
  if(n->type() != Number::Type::integer){
    throw zs_error("native func: integer->char: passed arg is not exact integer! (%s)",
                        stringify(n->type()));
  }

  return Lisp_ptr{static_cast<char>(n->get<Number::integer_type>())};
}

Lisp_ptr char_toupper(){
  return char_conversion("char-upcase",
                         [](char c){ return static_cast<char>(std::toupper(c)); });
}

Lisp_ptr char_tolower(){
  return char_conversion("char-downcase",
                         [](char c){ return static_cast<char>(std::tolower(c)); });
}
