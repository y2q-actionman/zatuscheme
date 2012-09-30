#ifndef PORT_HH
#define PORT_HH

#include <cstdio>
#include "lisp_ptr.hh"

Port* make_string_input_stream(const char*, size_t);

#endif // PORT_HH
