#include "test_util.hh"
#include "reader.hh"

Lisp_ptr read_from_string(SymTable& st, const char* s){
  FILE* f = fmemopen((void*)s, strlen(s), "r");
  if(!f){
    perror(__func__);
    return {};
  }

  Lisp_ptr p{read(st, f)};

  if(fclose(f) != 0){
    perror(__func__);
  }

  return p;
}


