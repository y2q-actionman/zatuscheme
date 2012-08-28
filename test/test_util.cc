#include <vector>
#include "test_util.hh"
#include "reader.hh"
#include "eval.hh"
#include "builtin.hh"

using namespace std;

Lisp_ptr read_from_string(const char* s){
  FILE* f = fmemopen((void*)s, strlen(s), "r");
  if(!f){
    perror(__func__);
    return {};
  }

  Lisp_ptr p{read(f)};

  if(fclose(f) != 0){
    perror(__func__);
  }

  return p;
}

Lisp_ptr eval_text(const char* s){
  auto exp = read_from_string(s);
  if(!exp){
    printf("[failed] read error on %s\n", s);
    return {};
  }

  VM.code.push(exp);
  eval();
  auto ret = VM.return_value;
  if(!ret){
    printf("[failed] eval error on %s\n", s);
    return {};
  }

  return ret;
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
  return VM.return_value;
}
