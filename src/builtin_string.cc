#include <utility>

#include "builtin_string.hh"
#include "lisp_ptr.hh"
#include "vm.hh"
#include "builtin_util.hh"
#include "procedure.hh"
#include "number.hh"

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

  VM.return_value = Lisp_ptr{str->operator[](ind)};
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
};

const size_t builtin_string_size = sizeof(builtin_string) / sizeof(builtin_string[0]);
