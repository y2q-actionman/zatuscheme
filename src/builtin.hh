#ifndef BUILTIN_HH
#define BUILTIN_HH

#include <iosfwd>
#include "decl.hh"
#include "procedure.hh"
#include "vm.hh"

#define CURRENT_INPUT_PORT_SYMNAME %current-input-port-value
#define CURRENT_OUTPUT_PORT_SYMNAME %current-output-port-value

void load_from_stream(std::istream&);
void install_builtin();

// type check predicate
namespace builtin {

template <Ptr_tag p>
Lisp_ptr type_check_pred(ZsArgs args){
  return Lisp_ptr{args[0].tag() == p};
}

} // namespace builtin

#endif // BUILTIN_HH
