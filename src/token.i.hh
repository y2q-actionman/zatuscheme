#ifndef TOKEN_I_HH
#define TOKEN_I_HH

#ifndef TOKEN_HH
#error "Please include via parent file"
#endif

#include <utility>
#include "decl.hh"

// Type mapping
template<>
struct to_type<Token::Type>{
  template<Token::Type t> struct get;
};

template<> template<>
struct to_type<Token::Type>::get<Token::Type::identifier>{
  typedef std::string type;
};

template<> template<>
struct to_type<Token::Type>::get<Token::Type::boolean>{
  typedef bool type;
};

template<> template<>
struct to_type<Token::Type>::get<Token::Type::number>{
  typedef Number type;
};

template<> template<>
struct to_type<Token::Type>::get<Token::Type::character>{
  typedef char type;
};

template<> template<>
struct to_type<Token::Type>::get<Token::Type::string>{
  typedef std::string type;
};

template<> template<>
struct to_type<Token::Type>::get<Token::Type::notation>{
  typedef Token::Notation type;
};


// std::string -> Token::Type is ambigious

template<>
inline constexpr
Token::Type to_tag<Token::Type, Number>(){
  return Token::Type::number;
}

template<>
inline constexpr
Token::Type to_tag<Token::Type, bool>(){
  return Token::Type::boolean;
}

template<>
inline constexpr
Token::Type to_tag<Token::Type, char>(){
  return Token::Type::character;
}

template<>
inline constexpr
Token::Type to_tag<Token::Type, Token::Notation>(){
  return Token::Type::notation;
}


// Token definitions
inline
Token::~Token(){
  using namespace std;

  switch(type_){
  case Type::identifier:
  case Type::string:
    str_.~string();
    break;

  case Type::number:
    num_.~Number();
    break;

  case Type::uninitialized:
  case Type::boolean:
  case Type::character:
  case Type::notation:
  default:
    break;
  }

  type_ = Type::uninitialized;
}

template<>
inline
std::string Token::get() const{
  switch(type_){
  case Type::identifier:
  case Type::string:
    return str_;
  default:
    return "";
  }
}

template<>
inline
Number Token::get() const{
  return (type_ == Type::number) ? num_ : Number{};
}

template<>
inline
bool Token::get() const{
  return (type_ == Type::boolean) ? b_ : false;
}

template<>
inline
char Token::get() const{
  return (type_ == Type::character) ? c_ : '\0';
}

template<>
inline
Token::Notation Token::get() const{
  return (type_ == Type::notation) ? not_ : Notation::unknown;
}

#endif // TOKEN_I_HH
