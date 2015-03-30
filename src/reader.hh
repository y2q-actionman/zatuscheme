#ifndef READER_HH
#define READER_HH

#include "decl.hh"

namespace zs {

// when reached EOF, returns eof-object if not in sequence.
Lisp_ptr read(std::istream&);

bool eof_object_p(Lisp_ptr);

} // namespace zs

#endif // READER_HH
