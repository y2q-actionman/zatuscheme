#include <functional>
#include <cctype>

#include "builtin_char.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "number.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

namespace {

zs_error char_type_check_failed(const char* func_name, Lisp_ptr p){
  return make_zs_error("native func: %s: arg is not %s! (%s)\n",
                       func_name, stringify(Ptr_tag::character), stringify(p.tag()));
}

template<typename Fun>
Lisp_ptr char_compare(const char* name, Fun&& fun){
  ZsArgs args{2};
  char c[2];

  for(auto i = 0; i < 2; ++i){
    c[i] = args[i].get<char>();
    if(!c[i]){
      throw char_type_check_failed(name, args[i]);
    }
  }

  return Lisp_ptr{fun(c[0], c[1])};
}

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
  

template<typename Fun>
struct ci_comparator{
  inline bool operator()(char c1, char c2) const{
    static constexpr Fun fun;
    return fun(tolower(c1), tolower(c2));
  }
};

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

  
template<typename Fun>
Lisp_ptr char_pred(Fun&& fun){
  ZsArgs args{1};

  auto c = args[0].get<char>();
  if(!c){
    return Lisp_ptr{false};
  }

  return Lisp_ptr{fun(c)};
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


template<typename Fun>
Lisp_ptr char_conversion(const char* name, Fun&& fun){
  ZsArgs args{1};

  auto c = args[0].get<char>();
  if(!c){
    throw char_type_check_failed(name, args[0]);
  }

  return Lisp_ptr{fun(c)};
}
  

Lisp_ptr char_to_int(){
  return char_conversion("char->integer",
                         [](char c){
                           return new Number(static_cast<Number::integer_type>(c));
                         });
}

Lisp_ptr char_from_int(){
  ZsArgs args{1};

  auto n = args[0].get<Number*>();
  if(!n){
    throw builtin_type_check_failed("integer->char", Ptr_tag::number, args[0]);
  }
  if(n->type() != Number::Type::integer){
    throw make_zs_error("native func: integer->char: passed arg is not exact integer! (%s)",
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

} // namespace


const BuiltinFunc
builtin_char[] = {
  {"char?", {
      type_check_pred<Ptr_tag::character>,
      {Calling::function, 1}}},

  {"char=?", {
      char_eq,
      {Calling::function, 2}}},
  {"char<?", {
      char_less,
      {Calling::function, 2}}},
  {"char>?", {
      char_greater,
      {Calling::function, 2}}},
  {"char<=?", {
      char_less_eq,
      {Calling::function, 2}}},
  {"char>=?", {
      char_greater_eq,
      {Calling::function, 2}}},

  {"char-ci=?", {
      char_ci_eq,
      {Calling::function, 2}}},
  {"char-ci<?", {
      char_ci_less,
      {Calling::function, 2}}},
  {"char-ci>?", {
      char_ci_greater,
      {Calling::function, 2}}},
  {"char-ci<=?", {
      char_ci_less_eq,
      {Calling::function, 2}}},
  {"char-ci>=?", {
      char_ci_greater_eq,
      {Calling::function, 2}}},

  {"char-alphabetic?", {
      char_isalpha,
      {Calling::function, 1}}},
  {"char-numeric?", {
      char_isdigit,
      {Calling::function, 1}}},
  {"char-whitespace?", {
      char_isspace,
      {Calling::function, 1}}},
  {"char-upper-case?", {
      char_isupper,
      {Calling::function, 1}}},
  {"char-lower-case?", {
      char_islower,
      {Calling::function, 1}}},

  {"char->integer", {
      char_to_int,
      {Calling::function, 1}}},
  {"integer->char", {
      char_from_int,
      {Calling::function, 1}}},
  {"char-upcase", {
      char_toupper,
      {Calling::function, 1}}},
  {"char-downcase", {
      char_tolower,
      {Calling::function, 1}}}
};

const size_t builtin_char_size = sizeof(builtin_char) / sizeof(builtin_char[0]);

