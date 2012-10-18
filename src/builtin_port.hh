#ifndef BUILTIN_PORT_HH
#define BUILTIN_PORT_HH

#include "builtin_util.hh"

extern const BuiltinFunc builtin_port[];
extern const size_t builtin_port_size;

void install_builtin_port_value();

extern const char* builtin_port_interpreted[];
extern const size_t builtin_port_interpreted_size;

#endif // BUILTIN_PORT_HH
