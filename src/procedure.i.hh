#ifndef PROCEDURE_I_HH
#define PROCEDURE_I_HH

#ifndef PROCEDURE_HH
#error "Please include via parent file"
#endif

namespace Procedure{

inline
bool is_procedure(Lisp_ptr p){
  return !!get_procinfo(p);
}

} // namespace Procedure

#endif // PROCEDURE_I_HH
