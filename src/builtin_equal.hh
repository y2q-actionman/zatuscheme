#ifndef BUILTIN_EQUAL_HH
#define BUILTIN_EQUAL_HH

#include "builtin.hh"

namespace zs {
namespace builtin {

Lisp_ptr eq(ZsArgs);
Lisp_ptr eqv(ZsArgs);
Lisp_ptr equal(ZsArgs);

}
}

#endif // BUILTIN_EQUAL_HH
