#include <vector>
#include "test_util.hh"
#include "reader.hh"
#include "eval.hh"
#include "builtin.hh"
#include "port.hh"

using namespace std;

Lisp_ptr read_from_string(const char* s){
  Port pr{(void*)s, strlen(s)};

  Lisp_ptr p{read(pr.stream())};

  return p;
}

Lisp_ptr eval_text(const char* s){
  auto exp = read_from_string(s);
  if(!exp){
    fprintf(zs::err, "[failed] read error on %s\n", s);
    return {};
  }

  VM.code.push(exp);
  eval();
  auto ret = VM.return_value[0];
  if(!ret){
    fprintf(zs::err, "[failed] eval error on %s\n", s);
    return {};
  }

  return ret;
}

bool read_eval_print_test(const char* input, const char* expect){
  auto e = eval_text(input);
  if(!e) return false;

  const auto fun = [input, expect](const char* s){
    fprintf(zs::err, "[failed] expected %s, but got %s (from: %s)\n",
            expect, s, input);
  };

  return test_on_print(e, expect, fun);
}

bool eqv(Lisp_ptr a, Lisp_ptr b){
  return zs_call("eqv?", {a, b}).get<bool>();
}
  
Lisp_ptr zs_call(const char* funcname, std::initializer_list<Lisp_ptr> args){
  vector<Cons> conses(args.size() + 1);

  conses[0].rplaca(Lisp_ptr(intern(VM.symtable, funcname)));
  conses[0].rplacd(Lisp_ptr(&conses[1]));

  auto i = next(begin(conses)), e = end(conses);
  auto args_i = begin(args), args_e = end(args);

  for(; i != e && args_i != args_e; ++i, ++args_i){
    i->rplaca(*args_i);

    auto n = next(i);
    if(n >= e){
      i->rplacd(Cons::NIL);
    }else{
      i->rplacd(Lisp_ptr(&*next(i)));
    }
  }
  assert(i == e && args_i == args_e);

  VM.code.push(Lisp_ptr(conses.data()));
  eval();
  return VM.return_value[0];
}


FILE* NULL_STREAM = NULL;

FILE* open_null_stream(){
  auto s = fopen("/dev/null", "w+b");
  if(!s) s = tmpfile();

  NULL_STREAM = s;
  return s;
}

with_null_stream::with_null_stream()
  : in(zs::in), out(zs::out), err(zs::err){
  if(!NULL_STREAM) open_null_stream();
  zs::in = NULL_STREAM;
  zs::out = NULL_STREAM;
  zs::err = NULL_STREAM;
}

with_null_stream::~with_null_stream(){
  zs::in = this->in;
  zs::out = this->out;
  zs::err = this->err;
}
