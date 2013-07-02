#ifndef PRINTER_HH
#define PRINTER_HH

#include <iosfwd>
#include "lisp_ptr.hh"

enum class PrintReadable{ f, t };

void print(std::ostream&, Lisp_ptr,
           PrintReadable flag = PrintReadable::f,
           int radix = 10);

// for debug;
inline
std::ostream& operator<<(std::ostream& o, Lisp_ptr p){
  print(o, p, PrintReadable::t);
  return o;
}

#endif //PRINTER_HH
