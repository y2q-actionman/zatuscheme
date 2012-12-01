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
#include "procedure.hh"
#include "number.hh"
#include "eval.hh"
#include "util.hh"

using namespace std;
using namespace Procedure;

namespace {

zs_error string_type_check_failed(const char* func_name, Lisp_ptr p){
  return make_zs_error("native func: %s: arg is not %s! (%s)\n",
                       func_name, stringify(Ptr_tag::string), stringify(p.tag()));
}

Lisp_ptr string_make(){
  ZsArgs args;

  auto num = args[0].get<Number*>();
  if(!num){
    throw builtin_type_check_failed("make-string", Ptr_tag::number, args[0]);
  }

  if(num->type() != Number::Type::integer){
    throw make_zs_error("native func: make-string: arg's number is not %s! (%s)\n",
                        stringify(Number::Type::integer), stringify(num->type()));
  }
  auto char_count = num->get<Number::integer_type>();

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
  ZsArgs args{1};
  auto str = args[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string-length", args[0]);
  }

  return {new Number(static_cast<Number::integer_type>(str->length()))};
}

Lisp_ptr string_ref(){
  ZsArgs args{2};
  auto str = args[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string-ref", args[0]);
  }

  auto num = args[1].get<Number*>();
  if(!num){
    throw builtin_type_check_failed("string-ref", Ptr_tag::number, args[1]);
  }

  if(num->type() != Number::Type::integer){
    throw make_zs_error("native func: string-ref: arg's number is not %s! (%s)\n",
                        stringify(Number::Type::integer), stringify(num->type()));
  }
  auto ind = num->get<Number::integer_type>();

  if(ind < 0 || ind >= str->length()){
    throw make_zs_error("native func: string-ref: index is out-of-bound ([0, %ld), supplied %ld\n",
                        str->length(), ind);
  }

  return Lisp_ptr{(*str)[ind]};
}

Lisp_ptr string_set(){
  ZsArgs args{3};
  auto str = args[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string-set!", args[0]);
  }

  auto num = args[1].get<Number*>();
  if(!num){
    throw builtin_type_check_failed("string-set!", Ptr_tag::number, args[1]);
  }

  if(num->type() != Number::Type::integer){
    throw make_zs_error("native func: string-set!: arg's number is not %s! (%s)\n",
                        stringify(Number::Type::integer), stringify(num->type()));
  }
  auto ind = num->get<Number::integer_type>();

  if(ind < 0 || ind >= str->length()){
    throw make_zs_error("native func: string-set!: index is out-of-bound ([0, %ld), supplied %ld\n",
                        str->length(), ind);
  }

  auto ch = args[2].get<char>();
  if(!ch){
    throw builtin_type_check_failed("string-set!", Ptr_tag::character, args[2]);
  }

  (*str)[ind] = ch;
  return Lisp_ptr{ch};
}

template<typename Fun>
Lisp_ptr string_compare(const char* name, Fun&& fun){
  ZsArgs args{2};
  String* str[2];

  for(auto i = 0; i < 2; ++i){
    str[i] = args[i].get<String*>();
    if(!str[i]){
      throw string_type_check_failed(name, args[i]);
    }
  }

  return Lisp_ptr{fun(*str[0], *str[1])};
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

template<typename Fun>
struct ci_compare{
  inline bool operator()(const String& s1, const String& s2) const {
    static constexpr Fun fun;
    return fun(strcasecmp(s1.c_str(), s2.c_str()), 0);
  }
};

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
  ZsArgs args{3};
  auto str = args[0].get<String*>();
  if(!str){
    throw string_type_check_failed("substring", args[0]);
  }

  Number::integer_type ind[2];

  for(int i = 1; i < 3; ++i){
    auto n = args[i].get<Number*>();
    if(!n){
      throw builtin_type_check_failed("substring", Ptr_tag::number, args[i]);
    }

    if(n->type() != Number::Type::integer){
      throw make_zs_error("native func: substring: arg's number is not %s! (%s)\n",
                          stringify(Number::Type::integer), stringify(n->type()));
    }
    ind[i-1] = n->get<Number::integer_type>();
  }


  if(!(0 <= ind[0] && ind[0] <= ind[1] && ind[1] <= str->length())){
    throw make_zs_error("native func: substring: index is out-of-bound ([0, %ld), supplied [%ld, %ld)\n",
                        str->length(), ind[0], ind[1]);
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
  ZsArgs args{1};
  auto str = args[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string->list", args[0]);
  }

  return make_cons_list(str->begin(), str->end());
}

Lisp_ptr string_from_list(){
  ZsArgs args{1};
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
  ZsArgs args{1};
  auto str = args[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string-copy", args[0]);
  }

  return {new String(*str)};
}

Lisp_ptr string_fill(){
  ZsArgs args{2};
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

} // namespace

const BuiltinFunc
builtin_string[] = {
  {"string?", {
      type_check_pred<Ptr_tag::string>,
      {Calling::function, 1}}},
  {"make-string", {
      string_make,
      {Calling::function, 1, 2}}},
  {"string", {
      string_string,
      {Calling::function, 0, Variadic::t}}},
  {"string-length", {
      string_length,
      {Calling::function, 1}}},
  {"string-ref", {
      string_ref,
      {Calling::function, 2}}},
  {"string-set!", {
      string_set,
      {Calling::function, 3}}},

  {"string=?", {
      string_equal,
      {Calling::function, 2}}},
  {"string<?", {
      string_less,
      {Calling::function, 2}}},
  {"string>?", {
      string_greater,
      {Calling::function, 2}}},
  {"string<=?", {
      string_less_eq,
      {Calling::function, 2}}},
  {"string>=?", {
      string_greater_eq,
      {Calling::function, 2}}},
  {"string-ci=?", {
      string_ci_equal,
      {Calling::function, 2}}},
  {"string-ci<?", {
      string_ci_less,
      {Calling::function, 2}}},
  {"string-ci>?", {
      string_ci_greater,
      {Calling::function, 2}}},
  {"string-ci<=?", {
      string_ci_less_eq,
      {Calling::function, 2}}},
  {"string-ci>=?", {
      string_ci_greater_eq,
      {Calling::function, 2}}},

  {"substring", {
      string_substr,
      {Calling::function, 3}}},
  {"string-append", {
      string_append,
      {Calling::function, 0, Variadic::t}}},

  {"string->list", {
      string_to_list,
      {Calling::function, 1}}},
  {"list->string", {
      string_from_list,
      {Calling::function, 1}}},

  {"string-copy", {
      string_copy,
      {Calling::function, 1}}},

  {"string-fill!", {
      string_fill,
      {Calling::function, 2}}}
};

const size_t builtin_string_size = sizeof(builtin_string) / sizeof(builtin_string[0]);
