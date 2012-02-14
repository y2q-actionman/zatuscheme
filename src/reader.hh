#ifndef READER_HH
#define READER_HH

#include <cstdio>
#include "lisp_ptr.hh"

class SymTable;

Lisp_ptr read(SymTable&, FILE*);

#endif // READER_HH
