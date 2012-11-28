#ifndef READER_HH
#define READER_HH

#include <iosfwd>
#include "lisp_ptr.hh"

Lisp_ptr read(std::istream&);

#endif // READER_HH
