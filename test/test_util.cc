#include "test_util.hh"
#include "reader.hh"
#include "eval.hh"
#include "builtin.hh"

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

  VM.code().push(exp);
  eval();
  auto ret = VM.return_value();
  if(!ret){
    printf("[failed] eval error on %s\n", s);
    return {};
  }

  return ret;
}

bool eql(Lisp_ptr a, Lisp_ptr b){
  VM.stack().push(Lisp_ptr(vm_op_arg_bottom));
  VM.stack().push(a);
  VM.stack().push(b);
  eql();
  return VM.return_value().get<bool>();
}
  
