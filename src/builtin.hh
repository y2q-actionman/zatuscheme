#ifndef BUILTIN_HH
#define BUILTIN_HH

#include <iosfwd>
#include "decl.hh"
#include "procedure.hh"
#include "vm.hh"

#define CURRENT_INPUT_PORT_SYMNAME %current-input-port-value
#define CURRENT_OUTPUT_PORT_SYMNAME %current-output-port-value

namespace zs {

void load_from_stream(std::istream&);
void install_builtin();

namespace builtin {

// type check predicate
template <Ptr_tag p>
Lisp_ptr type_check_pred(ZsArgs args){
  return Lisp_ptr{args[0].tag() == p};
}

} // namespace builtin
} // namespace zs

#endif // BUILTIN_HH
