#ifndef PRINTER_HH
#define PRINTER_HH

#include <iosfwd>
#include "lisp_ptr.hh"

namespace zs {

enum class PrintReadable{ f, t };

void print(std::ostream&, Lisp_ptr,
           PrintReadable flag = PrintReadable::f,
           int radix = 10);

// for debug;
std::ostream& operator<<(std::ostream&, Lisp_ptr);

} // namespace zs

#endif //PRINTER_HH
