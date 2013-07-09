#include <climits>
#include <cstring>
#include <string>

#include "builtin_string.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "zs_case.hh"
#include "zs_error.hh"
#include "zs_memory.hh"

using namespace std;

namespace {

template<typename Fun>
Lisp_ptr internal_string_cmp(ZsArgs&& args, Fun fun){
  const char* s[2];
  for(auto i = 0; i < 2; ++i){
    check_type(Ptr_tag::string, args[i]);
    s[i] = args[i].get<String*>()->c_str();
  }

  return Lisp_ptr{fun(s[0], s[1])};
}

} // namespace

namespace builtin {

Lisp_ptr internal_string_make(ZsArgs args){
  check_type(Ptr_tag::integer, args[0]);

  auto char_count = args[0].get<int>();
  if(char_count < 0){
    throw_builtin_range_check_failed(0, INT_MAX, char_count);
  }    

  check_type(Ptr_tag::character, args[1]);
  
  return {zs_new<String>(char_count, args[1].get<char>())};
}

Lisp_ptr string_length(ZsArgs args){
  check_type(Ptr_tag::string, args[0]);

  // TODO: add range check, and remove cast
  return Lisp_ptr{static_cast<int>(args[0].get<String*>()->length())};
}

Lisp_ptr string_ref(ZsArgs args){
  check_type(Ptr_tag::string, args[0]);
  auto str = args[0].get<String*>();

  check_type(Ptr_tag::integer, args[1]);
  auto ind = args[1].get<int>();

  if(ind < 0 || ind >= static_cast<signed>(str->length())){
    throw_builtin_range_check_failed(0, str->length(), ind);
  }

  return Lisp_ptr{(*str)[ind]};
}

Lisp_ptr string_set(ZsArgs args){
  check_type(Ptr_tag::string, args[0]);
  auto str = args[0].get<String*>();

  check_type(Ptr_tag::integer, args[1]);
  auto ind = args[1].get<int>();

  if(ind < 0 || ind >= static_cast<signed>(str->length())){
    throw_builtin_range_check_failed(0, str->length(), ind);
  }

  check_type(Ptr_tag::character, args[2]);
  auto ch = args[2].get<char>();

  (*str)[ind] = ch;
  return Lisp_ptr{ch};
}


Lisp_ptr internal_string_strcmp(ZsArgs args){
  return internal_string_cmp(move(args), strcmp);
}

Lisp_ptr internal_string_strcasecmp(ZsArgs args){
  return internal_string_cmp(move(args), zs_strcasecmp);
}

} // namespace builtin
