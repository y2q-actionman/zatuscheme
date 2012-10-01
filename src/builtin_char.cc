#include <functional>

#include "builtin_char.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"

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
  


constexpr BuiltinFunc
builtin_func[] = {
  {"char?", {
      type_check_pred<Ptr_tag::character>,
      Calling::function, {1, false}}},
  {"char=?", {
      char_eq,
      Calling::function, {2, false}}},
  {"char<?", {
      char_less,
      Calling::function, {2, false}}},
  {"char>?", {
      char_greater,
      Calling::function, {2, false}}},
  {"char<=?", {
      char_less_eq,
      Calling::function, {2, false}}},
  {"char>=?", {
      char_greater_eq,
      Calling::function, {2, false}}},
};

} // namespace

void install_builtin_char(){
  for(auto& e : builtin_func){
    VM.set(intern(VM.symtable, e.name), {&e.func});
  }
}
