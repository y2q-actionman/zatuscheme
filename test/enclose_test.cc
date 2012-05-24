#include <cstdlib>
#include <cassert>

#include "eval.hh"
#include "test_util.hh"
#include "builtin.hh"

static bool result = true;

int main(){
  // testing simple closure
  eval(read_from_string("(define x 1)"));
  eval(read_from_string("(define (hoge) x)"));

  if(!test_on_print(eval(read_from_string("(hoge)")),
                    "1",
                    [](const char* s){
                      printf("[failed] expected 1, but got %s\n", s);
                    })){
    result = false;
  }
  
  eval(read_from_string("(define x 100)"));

  if(!test_on_print(eval(read_from_string("(hoge)")),
                    "1",
                    [](const char* s){
                      printf("[failed] expected 1 from closure, but got %s\n", s);
                    })){
    result = false;
  }
  
       

  // testing defined variables

  // testing various syntaxes

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}

