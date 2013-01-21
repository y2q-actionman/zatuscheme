#ifndef LISP_PTR_I_HH
#define LISP_PTR_I_HH

#ifndef LISP_PTR_HH
#error "Please include via parent file"
#endif

#include <type_traits>

// Type mapping
template<>
struct to_type<Ptr_tag, Ptr_tag::boolean>{
  typedef bool type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::character>{
  typedef char type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::cons>{
  typedef Cons* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::symbol>{
  typedef Symbol* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::i_procedure>{
  typedef Procedure::IProcedure* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::n_procedure>{
  typedef const Procedure::IProcedure* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::continuation>{
  typedef const Procedure::Continuation* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::number>{
  typedef Number* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::string>{
  typedef String* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::vector>{
  typedef Vector* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::input_port>{
  typedef InputPort* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::output_port>{
  typedef OutputPort* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::env>{
  typedef Env* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::delay>{
  typedef Delay* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::syntactic_closure>{
  typedef SyntacticClosure* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::syntax_rules>{
  typedef Procedure::SyntaxRules* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::eq_hash_map>{
  typedef EqHashMap* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::vm_op>{
  typedef VMop type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::vm_argcount>{
  typedef int type;
};


template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, bool>(){
  return Ptr_tag::boolean;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, char>(){
  return Ptr_tag::character;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Cons*>(){
  return Ptr_tag::cons;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Symbol*>(){
  return Ptr_tag::symbol;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Procedure::IProcedure*>(){
  return Ptr_tag::i_procedure;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, const Procedure::NProcedure*>(){
  return Ptr_tag::n_procedure;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Procedure::Continuation*>(){
  return Ptr_tag::continuation;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Number*>(){
  return Ptr_tag::number;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, String*>(){
    return Ptr_tag::string;
  }

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Vector*>(){
  return Ptr_tag::vector;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, InputPort*>(){
  return Ptr_tag::input_port;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, OutputPort*>(){
  return Ptr_tag::output_port;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Env*>(){
  return Ptr_tag::env;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Delay*>(){
  return Ptr_tag::delay;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, SyntacticClosure*>(){
  return Ptr_tag::syntactic_closure;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Procedure::SyntaxRules*>(){
  return Ptr_tag::syntax_rules;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, EqHashMap*>(){
  return Ptr_tag::eq_hash_map;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, VMop>(){
  return Ptr_tag::vm_op;
}


// ptr class definitions

inline constexpr
Lisp_ptr::Lisp_ptr(bool b)
  : tag_(to_tag<Ptr_tag, bool>()), u_(b){}

inline constexpr
Lisp_ptr::Lisp_ptr(char c)
  : tag_(to_tag<Ptr_tag, char>()), u_(c){}

template<typename T>
inline constexpr
Lisp_ptr::Lisp_ptr(T p)
  : tag_(to_tag<Ptr_tag, T>()), u_(p){
  static_assert(!std::is_fundamental<T>::value,
                "Lisp_ptr cannot accept the specified type.");
}

inline constexpr
Lisp_ptr::Lisp_ptr(Ptr_tag p, int i)
  : tag_(p), u_(i){}


template<>
inline constexpr
bool Lisp_ptr::get<bool>() const {
  return (tag() == to_tag<Ptr_tag, bool>())
    ? u_.b_
    : operator bool(); // anything is #t, except #f and null
}

template<>
inline constexpr
char Lisp_ptr::get<char>() const {
  return (tag() == to_tag<Ptr_tag, char>())
    ? u_.c_ : '\0';
}

template<>
inline constexpr
VMop Lisp_ptr::get<VMop>() const {
  return (tag() == to_tag<Ptr_tag, VMop>()
     ? u_.f_ : nullptr);
}

template<>
inline constexpr
int Lisp_ptr::get<int>() const {
  return (tag() == Ptr_tag::vm_argcount ? u_.i_ : 0);
}

template<>
inline constexpr
void* Lisp_ptr::get<void*>() const{
  return u_.ptr_;
}

namespace lisp_ptr_detail {

template<typename T>
inline constexpr
T lisp_ptr_cast(void* p, const void*,
                typename std::enable_if<
                  !std::is_const<typename std::remove_pointer<T>::type>::value
                  >::type* = nullptr){
  return static_cast<T>(p);
}

template<typename T>
inline constexpr
T lisp_ptr_cast(void*, const void* cp,
                typename std::enable_if<
                  std::is_const<typename std::remove_pointer<T>::type>::value
                  >::type* = nullptr){
  return static_cast<T>(cp);
}

}

template<typename T>
inline constexpr
T Lisp_ptr::get() const {
  return (tag() == to_tag<Ptr_tag, T>())
    ? lisp_ptr_detail::lisp_ptr_cast<T>(u_.ptr_, u_.cptr_)
    : nullptr;
}

#endif // LISP_PTR_I_HH
