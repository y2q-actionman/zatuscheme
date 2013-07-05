#ifndef DECL_I_HH
#define DECL_I_HH

#ifndef DECL_HH
#error "Please include via parent file"
#endif

// Type mapping
template<>
struct to_type<Ptr_tag::boolean>{
  typedef bool type;
};

template<>
struct to_type<Ptr_tag::character>{
  typedef char type;
};

template<>
struct to_type<Ptr_tag::cons>{
  typedef Cons* type;
};

template<>
struct to_type<Ptr_tag::symbol>{
  typedef Symbol* type;
};

template<>
struct to_type<Ptr_tag::i_procedure>{
  typedef IProcedure* type;
};

template<>
struct to_type<Ptr_tag::n_procedure>{
  typedef const NProcedure* type;
};

template<>
struct to_type<Ptr_tag::continuation>{
  typedef const Continuation* type;
};

template<>
struct to_type<Ptr_tag::integer>{
  typedef int type;
};

template<>
struct to_type<Ptr_tag::rational>{
  typedef Rational* type;
};

template<>
struct to_type<Ptr_tag::real>{
  typedef double* type;
};

template<>
struct to_type<Ptr_tag::complex>{
  typedef Complex* type;
};

template<>
struct to_type<Ptr_tag::string>{
  typedef String* type;
};

template<>
struct to_type<Ptr_tag::vector>{
  typedef Vector* type;
};

template<>
struct to_type<Ptr_tag::input_port>{
  typedef InputPort* type;
};

template<>
struct to_type<Ptr_tag::output_port>{
  typedef OutputPort* type;
};

template<>
struct to_type<Ptr_tag::env>{
  typedef Env* type;
};

template<>
struct to_type<Ptr_tag::syntactic_closure>{
  typedef SyntacticClosure* type;
};

template<>
struct to_type<Ptr_tag::syntax_rules>{
  typedef SyntaxRules* type;
};

template<>
struct to_type<Ptr_tag::vm_op>{
  typedef VMop type;
};

template<>
struct to_type<Ptr_tag::vm_argcount>{
  typedef VMArgcount type;
};

template<>
struct to_type<Ptr_tag::notation>{
  typedef Notation type;
};


template<>
inline constexpr
Ptr_tag to_tag<bool>(){
  return Ptr_tag::boolean;
}

template<>
inline constexpr
Ptr_tag to_tag<char>(){
  return Ptr_tag::character;
}

template<>
inline constexpr
Ptr_tag to_tag<Cons*>(){
  return Ptr_tag::cons;
}

template<>
inline constexpr
Ptr_tag to_tag<Symbol*>(){
  return Ptr_tag::symbol;
}

template<>
inline constexpr
Ptr_tag to_tag<IProcedure*>(){
  return Ptr_tag::i_procedure;
}

template<>
inline constexpr
Ptr_tag to_tag<const NProcedure*>(){
  return Ptr_tag::n_procedure;
}

template<>
inline constexpr
Ptr_tag to_tag<Continuation*>(){
  return Ptr_tag::continuation;
}

template<>
inline constexpr
Ptr_tag to_tag<int>(){
  return Ptr_tag::integer;
}

template<>
inline constexpr
Ptr_tag to_tag<Rational*>(){
  return Ptr_tag::rational;
}

template<>
inline constexpr
Ptr_tag to_tag<double*>(){
  return Ptr_tag::real;
}

template<>
inline constexpr
Ptr_tag to_tag<Complex*>(){
  return Ptr_tag::complex;
}

template<>
inline constexpr
Ptr_tag to_tag<String*>(){
  return Ptr_tag::string;
}

template<>
inline constexpr
Ptr_tag to_tag<Vector*>(){
  return Ptr_tag::vector;
}

template<>
inline constexpr
Ptr_tag to_tag<InputPort*>(){
  return Ptr_tag::input_port;
}

template<>
inline constexpr
Ptr_tag to_tag<OutputPort*>(){
  return Ptr_tag::output_port;
}

template<>
inline constexpr
Ptr_tag to_tag<Env*>(){
  return Ptr_tag::env;
}

template<>
inline constexpr
Ptr_tag to_tag<SyntacticClosure*>(){
  return Ptr_tag::syntactic_closure;
}

template<>
inline constexpr
Ptr_tag to_tag<SyntaxRules*>(){
  return Ptr_tag::syntax_rules;
}

template<>
inline constexpr
Ptr_tag to_tag<VMop>(){
  return Ptr_tag::vm_op;
}

template<>
inline constexpr
Ptr_tag to_tag<VMArgcount>(){
  return Ptr_tag::vm_argcount;
}

template<>
inline constexpr
Ptr_tag to_tag<Notation>(){
  return Ptr_tag::notation;
}

#endif // DECL_I_HH
