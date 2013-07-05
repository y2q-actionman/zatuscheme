#ifndef DECL_HH
#define DECL_HH

#include <vector>
#include <string>
#include <iosfwd>
#include <complex>

// typedefs & declarations
class Lisp_ptr;

class Cons;
class Symbol;
class IProcedure;
class NProcedure;
class Continuation;
class Rational;
typedef std::complex<double> Complex;
typedef std::string String;
typedef std::vector<Lisp_ptr> Vector;
typedef std::istream InputPort;
typedef std::ostream OutputPort;
class Env;
class SyntacticClosure;
class SyntaxRules;
typedef void(*VMop)();
enum class Notation;

// Type tag
enum class Ptr_tag {
  undefined = 0,
    boolean,
    character,
    cons,
    symbol,
    i_procedure,
    n_procedure,
    continuation,
    integer,
    rational,
    real,
    complex,
    string,
    vector,
    input_port,
    output_port,
    env,
    syntactic_closure,
    syntax_rules,
    vm_op,
    vm_argcount,
    notation
    };

const char* stringify(Ptr_tag);

// Type mapping
template<Ptr_tag value>
struct to_type;

template<typename Arg>
Ptr_tag to_tag();

#include "decl.i.hh"

#endif // DECL_HH
