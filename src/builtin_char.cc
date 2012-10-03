#include <functional>
#include <cctype>

#include "builtin_char.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "number.hh"

using namespace std;
using namespace Procedure;

namespace {

void char_type_check_failed(const char* func_name, Lisp_ptr p){
  builtin_type_check_failed(func_name, Ptr_tag::cons, p);
}

template<typename Fun>
void char_compare(const char* name, Fun&& fun){
  auto args = pick_args<2>();
  char c[2];

  for(auto i = 0; i < 2; ++i){
    c[i] = args[i].get<char>();
    if(!c[i]){
      char_type_check_failed(name, args[i]);
      return;
    }
  }

  VM.return_value = Lisp_ptr{fun(c[0], c[1])};
}

void char_eq(){
  char_compare("char=?", std::equal_to<char>());
}

void char_less(){
  char_compare("char<?", std::less<char>());
}

void char_greater(){
  char_compare("char>?", std::greater<char>());
}

void char_less_eq(){
  char_compare("char<=?", std::less_equal<char>());
}

void char_greater_eq(){
  char_compare("char>=?", std::greater_equal<char>());
}
  

template<typename Fun>
struct ci_comparator{
  inline bool operator()(char c1, char c2) const{
    static constexpr Fun fun;
    return fun(tolower(c1), tolower(c2));
  }
};

void char_ci_eq(){
  char_compare("char-ci=?", ci_comparator<std::equal_to<char> >());
}

void char_ci_less(){
  char_compare("char-ci<?", ci_comparator<std::less<char> >());
}

void char_ci_greater(){
  char_compare("char-ci>?", ci_comparator<std::greater<char> >());
}

void char_ci_less_eq(){
  char_compare("char-ci<=?", ci_comparator<std::less_equal<char> >());
}

void char_ci_greater_eq(){
  char_compare("char-ci>=?", ci_comparator<std::greater_equal<char> >());
}

  
template<typename Fun>
void char_pred(Fun&& fun){
  auto arg1 = pick_args_1();

  auto c = arg1.get<char>();
  if(!c){
    VM.return_value = Lisp_ptr{false};
    return;
  }

  VM.return_value = Lisp_ptr{fun(c)};
}

void char_isalpha(){
  char_pred([](char c) -> bool{ return std::isalpha(c); });
}

void char_isdigit(){
  char_pred([](char c) -> bool{ return std::isdigit(c); });
}

void char_isspace(){
  char_pred([](char c) -> bool{ return std::isspace(c); });
}

void char_isupper(){
  char_pred([](char c) -> bool{ return std::isupper(c); });
}

void char_islower(){
  char_pred([](char c) -> bool{ return std::islower(c); });
}


template<typename Fun>
void char_conversion(const char* name, Fun&& fun){
  auto arg1 = pick_args_1();

  auto c = arg1.get<char>();
  if(!c){
    char_type_check_failed(name, arg1);
    return;
  }

  VM.return_value = Lisp_ptr{fun(c)};
}
  

void char_to_int(){
  char_conversion("char->integer",
                  [](char c){
                    return new Number(static_cast<Number::integer_type>(c));
                  });
}

void char_from_int(){
  auto arg1 = pick_args_1();

  auto n = arg1.get<Number*>();
  if(!n){
    builtin_type_check_failed("integer->char", Ptr_tag::number, arg1);
    return;
  }
  if(n->type() != Number::Type::integer){
    fprintf(zs::err, "native func: integer->char: passed arg is not exact integer! (%s)",
            stringify(n->type()));
    VM.return_value = {};
    return;
  }

  VM.return_value = Lisp_ptr{static_cast<char>(n->get<Number::integer_type>())};
}

void char_toupper(){
  char_conversion("char-upcase",
                  [](char c){ return static_cast<char>(std::toupper(c)); });
}

void char_tolower(){
  char_conversion("char-downcase",
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
