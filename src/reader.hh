#ifndef READER_HH
#define READER_HH

#include "decl.hh"

Lisp_ptr read(std::istream&);
bool eof_object_p(Lisp_ptr);

#endif // READER_HH
