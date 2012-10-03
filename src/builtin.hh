#ifndef BUILTIN_HH
#define BUILTIN_HH

#include "builtin_util.hh"

extern const BuiltinFunc builtin_misc[];
extern const size_t builtin_misc_size;

void install_builtin();

#endif // BUILTIN_HH
