#include <vector>
#include <utility>
#include <functional>
#include <string>
#include <cstring>
#include <algorithm>

#include "builtin_string.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "eval.hh"
#include "zs_error.hh"
#include "cons_util.hh"

using namespace std;

namespace {

zs_error string_type_check_failed(const char* func_name, Lisp_ptr p){
  return zs_error_arg1(func_name,
                       printf_string("arg is not %s!", stringify(Ptr_tag::string)),
                       {p});
}

template<typename Fun>
Lisp_ptr string_compare(const char* name, Fun&& fun){
  ZsArgs args;
  String* str[2];

  for(auto i = 0; i < 2; ++i){
    str[i] = args[i].get<String*>();
    if(!str[i]){
      throw string_type_check_failed(name, args[i]);
    }
  }

  return Lisp_ptr{fun(*str[0], *str[1])};
}

template<typename Fun>
struct ci_compare{
  inline bool operator()(const String& s1, const String& s2) const {
    static constexpr Fun fun;
    return fun(strcasecmp(s1.c_str(), s2.c_str()), 0);
  }
};

} // namespace

Lisp_ptr string_make(){
  ZsArgs args;

  if(args[0].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed("make-string", Ptr_tag::integer, args[0]);
  }
  auto char_count = args[0].get<int>();

  switch(args.size()){
  case 1:
    return {new String(char_count, '\0')};
  case 2: {
    auto c = args[1].get<char>();
    if(!c){
      throw builtin_type_check_failed("make-string", Ptr_tag::character, args[1]);
    }
    return {new String(char_count, c)};
  }
  default:
    throw builtin_argcount_failed("make-string", 1, 2, args.size());
  }
}

Lisp_ptr string_string(){
  ZsArgs args;

  String ret;
  for(auto i = args.begin(), e = args.end(); i != e; ++i){
    auto c = i->get<char>();
    if(!c){
      throw builtin_type_check_failed("string", Ptr_tag::character, *i);
    }

    ret.push_back(c);
  }

  return {new String(std::move(ret))};
}
  
Lisp_ptr string_length(){
  ZsArgs args;
  auto str = args[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string-length", args[0]);
  }

  // TODO: add range check, and remove cast
  return Lisp_ptr{Ptr_tag::integer,
      static_cast<int>(str->length())};
}

Lisp_ptr string_ref(){
  ZsArgs args;
  auto str = args[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string-ref", args[0]);
  }

  if(args[1].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed("string-ref", Ptr_tag::integer, args[1]);
  }
  auto ind = args[1].get<int>();

  if(ind < 0 || ind >= static_cast<signed>(str->length())){
    throw zs_error(printf_string("native func: string-ref: index is out-of-bound ([0, %ld), supplied %d\n",
                                 str->length(), ind));
  }

  return Lisp_ptr{(*str)[ind]};
}

Lisp_ptr string_set(){
  ZsArgs args;
  auto str = args[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string-set!", args[0]);
  }

  if(args[1].tag() != Ptr_tag::integer){
    throw builtin_type_check_failed("string-set!", Ptr_tag::integer, args[1]);
  }
  auto ind = args[1].get<int>();

  if(ind < 0 || ind >= static_cast<signed>(str->length())){
    throw zs_error(printf_string("native func: string-set!: index is out-of-bound ([0, %ld), supplied %d\n",
                                 str->length(), ind));
  }

  auto ch = args[2].get<char>();
  if(!ch){
    throw builtin_type_check_failed("string-set!", Ptr_tag::character, args[2]);
  }

  (*str)[ind] = ch;
  return Lisp_ptr{ch};
}


Lisp_ptr string_equal(){
  return string_compare("string=?", std::equal_to<std::string>());
}

Lisp_ptr string_less(){
  return string_compare("string<?", std::less<std::string>());
}

Lisp_ptr string_greater(){
  return string_compare("string>?", std::greater<std::string>());
}

Lisp_ptr string_less_eq(){
  return string_compare("string<=?", std::less_equal<std::string>());
}

Lisp_ptr string_greater_eq(){
  return string_compare("string>=?", std::greater_equal<std::string>());
}

Lisp_ptr string_ci_equal(){
  return string_compare("string-ci=?", ci_compare<std::equal_to<int> >());
}

Lisp_ptr string_ci_less(){
  return string_compare("string-ci<?", ci_compare<std::less<int> >());
}

Lisp_ptr string_ci_greater(){
  return string_compare("string-ci>?", ci_compare<std::greater<int> >());
}

Lisp_ptr string_ci_less_eq(){
  return string_compare("string-ci<=?", ci_compare<std::less_equal<int> >());
}

Lisp_ptr string_ci_greater_eq(){
  return string_compare("string-ci>=?", ci_compare<std::greater_equal<int> >());
}


Lisp_ptr string_substr(){
  ZsArgs args;
  auto str = args[0].get<String*>();
  if(!str){
    throw string_type_check_failed("substring", args[0]);
  }

  int ind[2];

  for(int i = 1; i < 3; ++i){
    if(args[i].tag() != Ptr_tag::integer){
      throw builtin_type_check_failed("substring", Ptr_tag::integer, args[i]);
    }
    ind[i-1] = args[i].get<int>();
  }

  if(!(0 <= ind[0] && ind[0] <= ind[1] && ind[1] <= static_cast<signed>(str->length()))){
    throw zs_error(printf_string("native func: substring: index is out-of-bound ([0, %ld), supplied [%d, %d)\n",
                                 str->length(), ind[0], ind[1]));
  }

  return {new String(str->substr(ind[0], ind[1] - ind[0]))};
}

Lisp_ptr string_append(){
  ZsArgs args;

  String ret;

  for(auto i = begin(args), e = end(args);
      i != e; ++i){
    auto str = i->get<String*>();
    if(!str){
      throw string_type_check_failed("string-append", *i);
    }

    ret.append(*str);
  }

  return {new String(std::move(ret))};
}

Lisp_ptr string_to_list(){
  ZsArgs args;
  auto str = args[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string->list", args[0]);
  }

  return make_cons_list(str->begin(), str->end());
}

Lisp_ptr string_from_list(){
  ZsArgs args;
  if(args[0].tag() != Ptr_tag::cons){
    throw builtin_type_check_failed("list->string", Ptr_tag::cons, args[0]);
  }

  String ret;
  
  for(auto p : args[0]){
    auto ch = p.get<char>();
    if(!ch){
      throw builtin_type_check_failed("list->string", Ptr_tag::character, p);
    }
    ret.push_back(ch);
  }

  return {new String(std::move(ret))};
}

Lisp_ptr string_copy(){
  ZsArgs args;
  auto str = args[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string-copy", args[0]);
  }

  return {new String(*str)};
}

Lisp_ptr string_fill(){
  ZsArgs args;
  auto str = args[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string-fill!", args[0]);
  }

  auto ch = args[1].get<char>();
  if(!ch){
    throw builtin_type_check_failed("string-fill!", Ptr_tag::character, args[1]);
  }

  std::fill(str->begin(), str->end(), ch);
  return {str};
}
