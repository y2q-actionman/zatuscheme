#include <vector>
#include <sstream>
#include <iostream>
#include <cstdlib>
#include "test_util.hh"
#include "reader.hh"
#include "printer.hh"
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


int result = true;

int check_r(const char* input, const char* expect){
  auto r = read_from_string(input);
  if(!r){
    result = false;
    cerr << "[failed] read error on " << input << "\n";
    return false;
  }

  const auto fun = [input, expect](const char* buf){
    cerr << "[failed] input:" << input << ", expected: " << expect << "\n"
         << "\treturned: " << buf << "\n";
  };

  auto ret = test_on_print(r, expect, fun);
  if(!ret) result = false;
  return result;
}

int check_r_undef(const char* input){
  with_expect_error([&]() -> void {
      auto ret = !read_from_string(input);
      if(!ret){
        result = false;
        cerr << "[failed] read:" << input << ", expected: (undefined)\n";
      }
    });
  return result;
}

int check_e(const char* input, const char* expect){
  auto e = eval_text(input);
  if(!e){
    result = false;
    return false;
  }

  const auto fun = [input, expect](const char* s){
    cerr << "[failed] expected " << expect
         << ", but got " << s << " (from: " << input << ")\n";
  };

  auto ret = test_on_print(e, expect, fun);
  if(!ret) result = false;
  return result;
}

int check_e_success(const char* input){
  auto ret = eval_text(input);
  if(!ret){
    result = false;
    cerr << "[failed] eval:" << input << ", expected: (success)\n";
  }
  return result;
}

int check_e_undef(const char* input){
  with_expect_error([&]() -> void {
      auto ret = !eval_text(input);
      if(!ret){
        result = false;
        cerr << "[failed] eval:" << input << ", expected: (undefined)\n";
      }
    });
  return result;
}

int check_er(const char* input, const char* expect){
  auto e = eval_text(input);
  if(!e){
    result = false;
    return false;
  }

  auto r = read_from_string(expect);
  assert(r);

  auto ret = eqv(e, r);
  if(!ret){
    result = false;
    cerr << "[failed] expected " << expect
         << ", but got ";
    print(cerr, e);
    cerr << " (from: " << input << ")\n";
  }
  return result;
}
