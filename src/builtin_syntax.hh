#ifndef BUILTIN_SYNTAX_HH
#define BUILTIN_SYNTAX_HH

#include "builtin_util.hh"

extern const BuiltinFunc builtin_syntax[];
extern const size_t builtin_syntax_size;

void install_builtin_syntax();

#endif // BUILTIN_SYNTAX_HH
