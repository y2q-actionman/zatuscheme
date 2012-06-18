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

bool eql(Lisp_ptr a, Lisp_ptr b){
  VM.stack().push(Lisp_ptr(static_cast<VM_op>(nullptr)));
  VM.stack().push(a);
  VM.stack().push(b);
  eql();
  return VM.return_value().get<bool>();
}
  
