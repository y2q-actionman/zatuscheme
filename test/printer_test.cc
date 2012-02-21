#include "printer.hh"

using namespace std;

static bool result;

int main(){
  result = true;

  Lisp_ptr p{'0'};

  print(stdout, p);

  return (result) ? EXIT_SUCCESS : EXIT_FAILURE;
}
