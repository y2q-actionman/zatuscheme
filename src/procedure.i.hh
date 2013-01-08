#ifndef PROCEDURE_I_HH
#define PROCEDURE_I_HH

#ifndef PROCEDURE_HH
#error "Please include via parent file"
#endif

namespace Procedure{

inline
bool is_procedure(Lisp_ptr p){
  auto tag = p.tag();
  return (tag == Ptr_tag::i_procedure)
    || (tag == Ptr_tag::n_procedure)
    || (tag == Ptr_tag::continuation)
    || (tag == Ptr_tag::syntax_rules);
}

} // namespace Procedure

#endif // PROCEDURE_I_HH
