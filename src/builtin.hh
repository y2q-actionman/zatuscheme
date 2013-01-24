#ifndef BUILTIN_HH
#define BUILTIN_HH

#include "decl.hh"
#include "procedure.hh"

void install_builtin();

// finding NProcedure with name
const Procedure::NProcedure* find_builtin_nproc(const char*);
const char* find_builtin_nproc_name(const Procedure::NProcedure*);

// builtin func struct
struct BuiltinFunc {
  const char* name;
  const Procedure::NProcedure func;

  constexpr BuiltinFunc(const char* n, const Procedure::NProcedure& f)
    : name(n), func(f){};
};

// LOAD from C++ world
void load(InputPort*);

#endif // BUILTIN_HH
