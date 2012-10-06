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

using namespace std;
using namespace Procedure;

namespace {

void string_type_check_failed(const char* func_name, Lisp_ptr p){
  builtin_type_check_failed(func_name, Ptr_tag::string, p);
}

void string_make(){
  auto arg1 = VM.stack.top();
  VM.stack.pop();

  auto num = arg1.get<Number*>();
  if(!num){
    builtin_type_check_failed("make-string", Ptr_tag::number, arg1);
    clean_args();
    return;
  }

  if(num->type() != Number::Type::integer){
    fprintf(zs::err, "native func: make-string: arg's number is not %s! (%s)\n",
            stringify(Number::Type::integer), stringify(num->type()));
    VM.return_value = {};
    clean_args();
    return;
  }
  auto char_count = num->get<Number::integer_type>();

  char ch;

  if(VM.stack.top().tag() == Ptr_tag::vm_op){
    VM.stack.pop();
    ch = '\0';
  }else{
    auto arg2 = pick_args_1();
    auto c = arg2.get<char>();
    if(!c){
      builtin_type_check_failed("make-string", Ptr_tag::character, arg2);
      return;
    }
    ch = c;
  }

  VM.return_value = {new String(char_count, ch)};
}

void string_string(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(VM.stack, args);

  String ret;
  for(auto i = args.begin(), e = args.end(); i != e; ++i){
    auto c = i->get<char>();
    if(!c){
      builtin_type_check_failed("string", Ptr_tag::character, *i);
      return;
    }

    ret.push_back(c);
  }

  VM.return_value = {new String(std::move(ret))};
}
  
void string_length(){
  auto arg1 = pick_args_1();
  auto str = arg1.get<String*>();
  if(!str){
    string_type_check_failed("string-length", arg1);
    return;
  }

  VM.return_value = {new Number(static_cast<Number::integer_type>(str->length()))};
}

void string_ref(){
  auto arg = pick_args<2>();
  auto str = arg[0].get<String*>();
  if(!str){
    string_type_check_failed("string-ref", arg[0]);
    return;
  }

  auto num = arg[1].get<Number*>();
  if(!num){
    builtin_type_check_failed("string-ref", Ptr_tag::number, arg[1]);
    return;
  }

  if(num->type() != Number::Type::integer){
    fprintf(zs::err, "native func: string-ref: arg's number is not %s! (%s)\n",
            stringify(Number::Type::integer), stringify(num->type()));
    VM.return_value = {};
    return;
  }
  auto ind = num->get<Number::integer_type>();

  if(ind < 0 || ind >= str->length()){
    fprintf(zs::err, "native func: string-ref: index is out-of-bound ([0, %ld), supplied %ld\n",
            str->length(), ind);
    VM.return_value = {};
    return;
  }

  VM.return_value = Lisp_ptr{(*str)[ind]};
}

void string_set(){
  auto arg = pick_args<3>();
  auto str = arg[0].get<String*>();
  if(!str){
    string_type_check_failed("string-set!", arg[0]);
    return;
  }

  auto num = arg[1].get<Number*>();
  if(!num){
    builtin_type_check_failed("string-set!", Ptr_tag::number, arg[1]);
    return;
  }

  if(num->type() != Number::Type::integer){
    fprintf(zs::err, "native func: string-set!: arg's number is not %s! (%s)\n",
            stringify(Number::Type::integer), stringify(num->type()));
    VM.return_value = {};
    return;
  }
  auto ind = num->get<Number::integer_type>();

  if(ind < 0 || ind >= str->length()){
    fprintf(zs::err, "native func: string-set!: index is out-of-bound ([0, %ld), supplied %ld\n",
            str->length(), ind);
    VM.return_value = {};
    return;
  }

  auto ch = arg[2].get<char>();
  if(!ch){
    builtin_type_check_failed("string-set!", Ptr_tag::character, arg[2]);
    return;
  }

  (*str)[ind] = ch;
  VM.return_value = Lisp_ptr{ch};
}

template<typename Fun>
void string_compare(const char* name, Fun&& fun){
  auto args = pick_args<2>();
  String* str[2];

  for(auto i = 0; i < 2; ++i){
    str[i] = args[i].get<String*>();
    if(!str[i]){
      string_type_check_failed(name, args[i]);
      return;
    }
  }

  VM.return_value = Lisp_ptr{fun(*str[0], *str[1])};
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
    string_type_check_failed("substring", arg[0]);
    return;
  }

  Number::integer_type ind[2];

  for(int i = 1; i < 3; ++i){
    auto n = arg[i].get<Number*>();
    if(!n){
      builtin_type_check_failed("substring", Ptr_tag::number, arg[i]);
      return;
    }

    if(n->type() != Number::Type::integer){
      fprintf(zs::err, "native func: substring: arg's number is not %s! (%s)\n",
              stringify(Number::Type::integer), stringify(n->type()));
      VM.return_value = {};
      return;
    }
    ind[i-1] = n->get<Number::integer_type>();
  }


  if(!(0 <= ind[0] && ind[0] <= ind[1] && ind[1] <= str->length())){
    fprintf(zs::err, "native func: substring: index is out-of-bound ([0, %ld), supplied [%ld, %ld)\n",
            str->length(), ind[0], ind[1]);
    VM.return_value = {};
    return;
  }

  auto ret = str->substr(ind[0], ind[1] - ind[0]);
  VM.return_value = {new String(std::move(ret))};
}

void string_append(){
  std::vector<Lisp_ptr> args;
  stack_to_vector(VM.stack, args);

  String ret;

  for(auto i = begin(args), e = end(args);
      i != e; ++i){
    auto str = i->get<String*>();
    if(!str){
      string_type_check_failed("string-append", *i);
      return;
    }

    ret.append(*str);
  }

  VM.return_value = {new String(std::move(ret))};
}

void string_to_list(){
  auto arg1 = pick_args_1();
  auto str = arg1.get<String*>();
  if(!str){
    string_type_check_failed("string->list", arg1);
    return;
  }

  VM.return_value = make_cons_list(str->begin(), str->end());
}

void string_from_list(){
  auto arg = pick_args_1();
  if(arg.tag() != Ptr_tag::cons){
    builtin_type_check_failed("list->string", Ptr_tag::cons, arg);
    return;
  }

  bool not_char_found = false;
  String ret;
  
  do_list(arg,
          [&](Cons* c) -> bool{
            auto ch = c->car().get<char>();
            if(!ch){
              builtin_type_check_failed("list->string", Ptr_tag::character, c->car());
              not_char_found = true;
              return false;
            }
              
            ret.push_back(ch);
            return true;
          },
          [](Lisp_ptr){});

  if(not_char_found){
    VM.return_value = {};
  }else{
    VM.return_value = {new String(std::move(ret))};
  }
}

void string_copy(){
  auto arg1 = pick_args_1();
  auto str = arg1.get<String*>();
  if(!str){
    string_type_check_failed("string-copy", arg1);
    return;
  }

  VM.return_value = {new String(*str)};
}

void string_fill(){
  auto arg = pick_args<2>();
  auto str = arg[0].get<String*>();
  if(!str){
    string_type_check_failed("string-fill!", arg[0]);
    return;
  }

  auto ch = arg[1].get<char>();
  if(!ch){
    builtin_type_check_failed("string-fill!", Ptr_tag::character, arg[1]);
    return;
  }

  std::fill(str->begin(), str->end(), ch);
  VM.return_value = {str};
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
