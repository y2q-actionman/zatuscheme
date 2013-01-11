#ifndef BUILTIN_HH
#define BUILTIN_HH

#include "decl.hh"

void install_builtin();

void load(InputPort*);

const Procedure::NProcedure* find_builtin_nproc(const char*);
const char* find_builtin_nproc_name(const Procedure::NProcedure*);

#endif // BUILTIN_HH
