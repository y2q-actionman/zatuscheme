#ifndef DECL_HH
#define DECL_HH

#include <vector>
#include <string>
#include <iosfwd>
#include <unordered_map>

// typedefs & declarations
class Lisp_ptr;

class Cons;
class Symbol;
namespace Procedure{
  class IProcedure;
  class NProcedure;
  class Continuation;
  class SyntaxRules;
}
class Number;
typedef std::string String;
typedef std::vector<Lisp_ptr> Vector;
typedef std::istream InputPort;
typedef std::ostream OutputPort;
class Env;
class Delay;
class SyntacticClosure;
class eq_hash_obj; // for EqHashMap
class eq_obj; // for EqHashMap
typedef std::unordered_map<Lisp_ptr, Lisp_ptr, eq_hash_obj, eq_obj> EqHashMap;
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
    eq_hash_map,
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
