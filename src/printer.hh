#ifndef PRINTER_HH
#define PRINTER_HH

#include "decl.hh"

enum class print_human_readable{ f, t };

void print(std::ostream&, Lisp_ptr,
           print_human_readable flag = print_human_readable::f);

#endif //PRINTER_HH
