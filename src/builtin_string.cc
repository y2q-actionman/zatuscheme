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

void string_make(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(vm.stack, args);

  auto num = args[0].get<Number*>();
  if(!num){
    throw builtin_type_check_failed("make-string", Ptr_tag::number, args[0]);
  }

  if(num->type() != Number::Type::integer){
    throw make_zs_error("native func: make-string: arg's number is not %s! (%s)\n",
                        stringify(Number::Type::integer), stringify(num->type()));
  }
  auto char_count = num->get<Number::integer_type>();

  char ch;

  switch(args.size()){
  case 1:
    ch = '\0';
    break;
  case 2: {
    auto c = args[1].get<char>();
    if(!c){
      throw builtin_type_check_failed("make-string", Ptr_tag::character, args[1]);
    }
    ch = c;
    break;
  }
  default:
    throw builtin_variadic_argcount_failed("make-string", 2);
  }

  vm.return_value[0] = {new String(char_count, ch)};
}

void string_string(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(vm.stack, args);

  String ret;
  for(auto i = args.begin(), e = args.end(); i != e; ++i){
    auto c = i->get<char>();
    if(!c){
      throw builtin_type_check_failed("string", Ptr_tag::character, *i);
    }

    ret.push_back(c);
  }

  vm.return_value[0] = {new String(std::move(ret))};
}
  
void string_length(){
  auto arg1 = pick_args_1();
  auto str = arg1.get<String*>();
  if(!str){
    throw string_type_check_failed("string-length", arg1);
  }

  vm.return_value[0] = {new Number(static_cast<Number::integer_type>(str->length()))};
}

void string_ref(){
  auto arg = pick_args<2>();
  auto str = arg[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string-ref", arg[0]);
  }

  auto num = arg[1].get<Number*>();
  if(!num){
    throw builtin_type_check_failed("string-ref", Ptr_tag::number, arg[1]);
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

  vm.return_value[0] = Lisp_ptr{(*str)[ind]};
}

void string_set(){
  auto arg = pick_args<3>();
  auto str = arg[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string-set!", arg[0]);
  }

  auto num = arg[1].get<Number*>();
  if(!num){
    throw builtin_type_check_failed("string-set!", Ptr_tag::number, arg[1]);
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

  auto ch = arg[2].get<char>();
  if(!ch){
    throw builtin_type_check_failed("string-set!", Ptr_tag::character, arg[2]);
  }

  (*str)[ind] = ch;
  vm.return_value[0] = Lisp_ptr{ch};
}

template<typename Fun>
void string_compare(const char* name, Fun&& fun){
  auto args = pick_args<2>();
  String* str[2];

  for(auto i = 0; i < 2; ++i){
    str[i] = args[i].get<String*>();
    if(!str[i]){
      throw string_type_check_failed(name, args[i]);
    }
  }

  vm.return_value[0] = Lisp_ptr{fun(*str[0], *str[1])};
}

void string_equal(){
  string_compare("string=?", std::equal_to<std::string>());
}

void string_less(){
  string_compare("string<?", std::less<std::string>());
}

void string_greater(){
  string_compare("string>?", std::greater<std::string>());
}

void string_less_eq(){
  string_compare("string<=?", std::less_equal<std::string>());
}

void string_greater_eq(){
  string_compare("string>=?", std::greater_equal<std::string>());
}

template<typename Fun>
struct ci_compare{
  inline bool operator()(const String& s1, const String& s2) const {
    static constexpr Fun fun;
    return fun(strcasecmp(s1.c_str(), s2.c_str()), 0);
  }
};

void string_ci_equal(){
  string_compare("string-ci=?", ci_compare<std::equal_to<int> >());
}

void string_ci_less(){
  string_compare("string-ci<?", ci_compare<std::less<int> >());
}

void string_ci_greater(){
  string_compare("string-ci>?", ci_compare<std::greater<int> >());
}

void string_ci_less_eq(){
  string_compare("string-ci<=?", ci_compare<std::less_equal<int> >());
}

void string_ci_greater_eq(){
  string_compare("string-ci>=?", ci_compare<std::greater_equal<int> >());
}


void string_substr(){
  auto arg = pick_args<3>();
  auto str = arg[0].get<String*>();
  if(!str){
    throw string_type_check_failed("substring", arg[0]);
  }

  Number::integer_type ind[2];

  for(int i = 1; i < 3; ++i){
    auto n = arg[i].get<Number*>();
    if(!n){
      throw builtin_type_check_failed("substring", Ptr_tag::number, arg[i]);
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

  auto ret = str->substr(ind[0], ind[1] - ind[0]);
  vm.return_value[0] = {new String(std::move(ret))};
}

void string_append(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(vm.stack, args);

  String ret;

  for(auto i = begin(args), e = end(args);
      i != e; ++i){
    auto str = i->get<String*>();
    if(!str){
      throw string_type_check_failed("string-append", *i);
    }

    ret.append(*str);
  }

  vm.return_value[0] = {new String(std::move(ret))};
}

void string_to_list(){
  auto arg1 = pick_args_1();
  auto str = arg1.get<String*>();
  if(!str){
    throw string_type_check_failed("string->list", arg1);
  }

  vm.return_value[0] = make_cons_list(str->begin(), str->end());
}

void string_from_list(){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::cons){
    throw builtin_type_check_failed("list->string", Ptr_tag::cons, arg);
  }

  String ret;
  
  do_list(arg,
          [&](Cons* c) -> bool{
            auto ch = c->car().get<char>();
            if(!ch){
              throw builtin_type_check_failed("list->string", Ptr_tag::character, c->car());
            }
              
            ret.push_back(ch);
            return true;
          },
          [](Lisp_ptr){});

  vm.return_value[0] = {new String(std::move(ret))};
}

void string_copy(){
  auto arg1 = pick_args_1();
  auto str = arg1.get<String*>();
  if(!str){
    throw string_type_check_failed("string-copy", arg1);
  }

  vm.return_value[0] = {new String(*str)};
}

void string_fill(){
  auto arg = pick_args<2>();
  auto str = arg[0].get<String*>();
  if(!str){
    throw string_type_check_failed("string-fill!", arg[0]);
  }

  auto ch = arg[1].get<char>();
  if(!ch){
    throw builtin_type_check_failed("string-fill!", Ptr_tag::character, arg[1]);
  }

  std::fill(str->begin(), str->end(), ch);
  vm.return_value[0] = {str};
}


} // namespace

const BuiltinFunc
builtin_string[] = {
  {"string?", {
      type_check_pred<Ptr_tag::string>,
      {Calling::function, 1}}},
  {"make-string", {
      string_make,
      {Calling::function, 1, Variadic::t}}},
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
