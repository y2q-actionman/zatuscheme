#ifndef BUILTIN_HH
#define BUILTIN_HH

#include <iosfwd>
#include "decl.hh"
#include "procedure.hh"
#include "vm.hh"

#define CURRENT_INPUT_PORT_SYMNAME *current-input-port-value*
#define CURRENT_OUTPUT_PORT_SYMNAME *current-output-port-value*

void load_internal(std::istream&);
void install_builtin();

// finding NProcedure with name
const NProcedure* find_builtin_nproc(const char*);
const char* find_builtin_nproc_name(const NProcedure*);

// builtin func struct
struct BuiltinNProc {
  const char* name;
  const NProcedure func;

  // this constructor is required for static initialization
  constexpr BuiltinNProc(const char* n, const NProcedure& f)
    : name(n), func(f){};
};

// type check predicate
namespace builtin {

template <Ptr_tag p>
Lisp_ptr type_check_pred(ZsArgs args){
  return Lisp_ptr{args[0].tag() == p};
}

} // namespace builtin

#endif // BUILTIN_HH
