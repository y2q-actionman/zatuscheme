#ifndef DECL_HH
#define DECL_HH

#include <vector>
#include <string>
#include <iosfwd>

// typedefs & declarations
class Lisp_ptr;

class Cons;
class Symbol;
class IProcedure;
class NProcedure;
class Continuation;
class Number;
typedef std::string String;
typedef std::vector<Lisp_ptr> Vector;
typedef std::istream InputPort;
typedef std::ostream OutputPort;
class Env;
class Delay;
class SyntacticClosure;
class SyntaxRules;
typedef void(*VMop)();


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
    number,
    string,
    vector,
    input_port,
    output_port,
    env,
    delay,
    syntactic_closure,
    syntax_rules,
    vm_op,
    vm_argcount
    };


// declares generic facilities.

// Type mapping - provided by overload
template<typename EnumType, EnumType value>
struct to_type;
// usage
//   typedef typename to_type<Number::Type, Number::Type::integer>::type HogeT;


template<typename enum_type, typename Arg>
enum_type to_tag();
// usage
//   to_tag<Token::Type, std::string>();


// provided by overload
//   template<typename Enum_type>
//   const char* stringify(Ennum_type);

#endif // DECL_HH
