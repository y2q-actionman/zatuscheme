#include <vector>
#include <utility>
#include <functional>
#include <string>
#include <cstring>
#include <algorithm>

#include "builtin_string.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "eval.hh"
#include "zs_error.hh"
#include "cons_util.hh"
#include "zs_memory.hh"

using namespace std;

namespace {

template<typename Fun>
Lisp_ptr internal_string_cmp(ZsArgs args, Fun fun){
  for(auto p : args){
    if(p.tag() != Ptr_tag::string){
      throw builtin_type_check_failed(nullptr, Ptr_tag::string, p);
    }
  }

  return Lisp_ptr{Ptr_tag::integer,
      fun(args[0].get<String*>()->c_str(),
          args[1].get<String*>()->c_str())};
}

} // namespace

namespace builtin {

Lisp_ptr string_make(ZsArgs args){
  if(args[0].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed(nullptr, Ptr_tag::integer, args[0]);
  }

  auto char_count = args[0].get<int>();
  if(char_count < 0){
    throw zs_error("passed size is less than 0");
  }    

  if(args[1].tag() != Ptr_tag::character){
    throw builtin_type_check_failed(nullptr, Ptr_tag::character, args[1]);
  }
  
  return {zs_new<String>(char_count, args[1].get<char>())};
}

Lisp_ptr string_length(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(nullptr, Ptr_tag::string, args[0]);
  }

  // TODO: add range check, and remove cast
  return Lisp_ptr{Ptr_tag::integer,
      static_cast<int>(str->length())};
}

Lisp_ptr string_ref(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(nullptr, Ptr_tag::string, args[0]);
  }

  if(args[1].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed(nullptr, Ptr_tag::integer, args[1]);
  }
  auto ind = args[1].get<int>();

  if(ind < 0 || ind >= static_cast<signed>(str->length())){
    throw builtin_range_check_failed(str->length(), ind);
  }

  return Lisp_ptr{(*str)[ind]};
}

Lisp_ptr string_set(ZsArgs args){
  auto str = args[0].get<String*>();
  if(!str){
    throw builtin_type_check_failed(nullptr, Ptr_tag::string, args[0]);
  }

  if(args[1].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed(nullptr, Ptr_tag::integer, args[1]);
  }
  auto ind = args[1].get<int>();

  if(ind < 0 || ind >= static_cast<signed>(str->length())){
    throw builtin_range_check_failed(str->length(), ind);
  }

  auto ch = args[2].get<char>();
  if(!ch){
    throw builtin_type_check_failed(nullptr, Ptr_tag::character, args[2]);
  }

  (*str)[ind] = ch;
  return Lisp_ptr{ch};
}


Lisp_ptr internal_string_strcmp(ZsArgs args){
  return internal_string_cmp(move(args), strcmp);
}

Lisp_ptr internal_string_strcasecmp(ZsArgs args){
  return internal_string_cmp(move(args), strcasecmp);
}

} // namespace builtin
