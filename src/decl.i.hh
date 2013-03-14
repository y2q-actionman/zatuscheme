#ifndef DECL_I_HH
#define DECL_I_HH

#ifndef DECL_HH
#error "Please include via parent file"
#endif

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
  typedef IProcedure* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::n_procedure>{
  typedef const NProcedure* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::continuation>{
  typedef const Continuation* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::number>{
  typedef Number* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::integer>{
  typedef int type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::real>{
  typedef double* type;
};

template<>
struct to_type<Ptr_tag, Ptr_tag::complex>{
  typedef std::complex<double>* type;
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
  typedef SyntaxRules* type;
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
Ptr_tag to_tag<Ptr_tag, IProcedure*>(){
  return Ptr_tag::i_procedure;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, const NProcedure*>(){
  return Ptr_tag::n_procedure;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Continuation*>(){
  return Ptr_tag::continuation;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, Number*>(){
  return Ptr_tag::number;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, double*>(){
  return Ptr_tag::real;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, std::complex<double>*>(){
  return Ptr_tag::complex;
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
Ptr_tag to_tag<Ptr_tag, SyntaxRules*>(){
  return Ptr_tag::syntax_rules;
}

template<>
inline constexpr
Ptr_tag to_tag<Ptr_tag, VMop>(){
  return Ptr_tag::vm_op;
}

#endif // DECL_I_HH
