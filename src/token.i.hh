#ifndef TOKEN_I_HH
#define TOKEN_I_HH

#ifndef TOKEN_HH
#error "Please include via parent file"
#endif

#include <utility>
#include "decl.hh"
#include "util.hh"

// Type mapping
template<Token::Type t, typename T>
T to_type() = delete;

template<>
std::string to_type<Token::Type::identifier>() = delete;

template<>
bool to_type<Token::Type::boolean>() = delete;

template<>
Number to_type<Token::Type::number>() = delete;

template<>
char to_type<Token::Type::character>() = delete;

template<>
std::string to_type<Token::Type::string>() = delete;

template<>
Token::Notation to_type<Token::Type::notation>() = delete;


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
    UNEXP_CONVERSION("(no string token)", "string");
  }
}

template<>
inline
Number Token::get() const{
  if(type_ == Type::number)
    return num_;
  else
    UNEXP_CONVERSION("(no number token)", "number");
}

template<>
inline
bool Token::get() const{
  if(type_ == Type::boolean)
    return b_;
  else
    UNEXP_CONVERSION("(no boolean token)", "boolean");
}

template<>
inline
char Token::get() const{
  if(type_ == Type::character)
    return c_;
  else
    UNEXP_CONVERSION("(no char token)", "char");
}

template<>
inline
Token::Notation Token::get() const{
  if(type_ == Type::notation)
    return not_;
  else
    UNEXP_CONVERSION("(no notation token)", "notation");
}

#endif // TOKEN_I_HH
