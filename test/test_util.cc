#include <vector>
#include <sstream>
#include <iostream>
#include <cstdlib>
#include "test_util.hh"
#include "reader.hh"
#include "printer.hh"
#include "eval.hh"
#include "builtin_util.hh"
#include "vm.hh"

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


int RESULT = EXIT_SUCCESS;

struct print_check_error{
  string result;
};

static
void print_check(Lisp_ptr input, const char* expect){
  std::stringstream ss;
  print(ss, input);

  auto evaled = ss.str();

  if(expect != evaled){
    RESULT = EXIT_FAILURE;
    throw print_check_error{evaled};
  }
}

bool check_p(Lisp_ptr input, const char* expect){
  try{
    print_check(input, expect);
    return true;
  }catch(const print_check_error& e){
    cerr << "[failed] expected: " << expect << "\n"
         << "\treturned: " << e.result << "\n";
    RESULT = EXIT_FAILURE;
    return false;
  }
}

bool check_p_success(Lisp_ptr input){
  stringstream ss;
  try{
    print(ss, input);
    return true;
  }catch(...){
    cerr << "[failed] print error: ";
    print(cerr, input);
    cerr << "\n";
    return false;
  }
}

bool check_r(const char* input, const char* expect){
  auto r = read_from_string(input);
  if(!r){
    cerr << "[failed] read error on " << input << "\n";
    RESULT = EXIT_FAILURE;
    return false;
  }

  try{
    print_check(r, expect);
    return true;
  }catch(const print_check_error& e){
    cerr << "[failed] input:" << input << ", expected: " << expect << "\n"
         << "\treturned: " << e.result << "\n";
    RESULT = EXIT_FAILURE;
    return false;
  };
}

bool check_r_undef(const char* input){
  Lisp_ptr ret;
  with_expect_error([&](){ ret = read_from_string(input); });

  if(!ret){
    return true;
  }else{
        cerr << "[failed] read:" << input << ", expected: (undefined)\n";
        RESULT = EXIT_FAILURE;
        return false;
  }
}

bool check_e(const char* input, const char* expect){
  auto e = eval_text(input);
  if(!e){
    RESULT = EXIT_FAILURE;
    return false;
  }

  try{
    print_check(e, expect);
    return true;
  }catch(const print_check_error& err){
    cerr << "[failed] expected " << expect
         << ", but got " << err.result << " (from: " << input << ")\n";
    RESULT = EXIT_FAILURE;
    return false;
  }
}

bool check_e_success(const char* input){
  if(eval_text(input)){
    return true;
  }else{
    cerr << "[failed] eval:" << input << ", expected: (success)\n";
    RESULT = EXIT_FAILURE;
    return false;
  }
}

bool check_e_undef(const char* input){
  Lisp_ptr ret;
  with_expect_error([&](){ ret = eval_text(input); });

  if(!ret){
    return true;
  }else{
    cerr << "[failed] eval:" << input << ", expected: (undefined)\n";
    RESULT = EXIT_FAILURE;
    return false;
  }
}

bool check_er(const char* input, const char* expect){
  auto e = eval_text(input);
  if(!e){
    RESULT = EXIT_FAILURE;
    return false;
  }

  auto r = read_from_string(expect);
  assert(r);

  auto ret = eqv(e, r);
  if(!ret){
    cerr << "[failed] expected " << expect << ", but got ";
    print(cerr, e);
    cerr << " (from: " << input << ")\n";
    RESULT = EXIT_FAILURE;
    return false;
  }
  return true;
}
