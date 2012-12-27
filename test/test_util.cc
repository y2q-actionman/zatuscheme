#include <vector>
#include <sstream>
#include <iostream>
#include "test_util.hh"
#include "reader.hh"
#include "eval.hh"
#include "builtin_util.hh"

using namespace std;

Lisp_ptr read_from_string(const char* s){
  stringstream ss(s);
  return read(ss);
}

Lisp_ptr eval_text(const char* s){
  auto exp = read_from_string(s);
  if(!exp){
    cerr << "[failed] read error on " << s << "\n";
    return {};
  }

  vm.code.push_back(exp);
  eval();
  auto ret = vm.return_value[0];
  if(!ret){
    cerr << "[failed] eval error on " << s << "\n";
    return {};
  }

  return ret;
}

bool read_eval_print_test(const char* input, const char* expect){
  auto e = eval_text(input);
  if(!e) return false;

  const auto fun = [input, expect](const char* s){
    cerr << "[failed] expected " << expect
         << ", but got " << s << " (from: " << input << ")\n";
  };

  return test_on_print(e, expect, fun);
}

bool eqv(Lisp_ptr a, Lisp_ptr b){
  auto eqv_ret = zs_call({intern(vm.symtable(), "eqv?"), a, b});
  return eqv_ret.get<bool>();
}
  
Lisp_ptr zs_call(std::initializer_list<Lisp_ptr> args){
  vm.code.push_back(make_cons_list(args));
  eval();
  return vm.return_value[0];
}


namespace test_util_detail {

with_null_stream::with_null_stream()
  : orig_obuf(std::cerr.rdbuf()){
  cerr.rdbuf(new std::stringbuf());
}

with_null_stream::~with_null_stream(){
  cerr.rdbuf(orig_obuf);
}

} // namespace test_util_detail
