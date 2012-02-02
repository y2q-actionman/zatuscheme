#ifndef READER_HH
#define READER_HH

#include <iosfwd>
#include "lisp_ptr.hh"

class SymTable;

Lisp_ptr read(SymTable&, std::istream&);

#endif // READER_HH
