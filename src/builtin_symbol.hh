#ifndef BUILTIN_SYMBOL_HH
#define BUILTIN_SYMBOL_HH

#include "builtin_util.hh"

extern const BuiltinFunc builtin_symbol[];
extern const size_t builtin_symbol_size;

void install_builtin_symbol();

#endif // BUILTIN_SYMBOL_HH
