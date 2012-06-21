#ifndef TOKEN_I_HH
#define TOKEN_I_HH

#ifndef TOKEN_HH
#error "Please include via parent file"
#endif

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
Token::Type to_tag<Token::Type, std::string>() = delete;

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
template<>
inline
std::string Token::get() const{
  if(type_ == Type::identifier || type_ == Type::string)
    return str_;
  else
    UNEXP_CONVERSION("(?)", "string");
}

template<>
inline
Number Token::get() const{
  if(type_ == Type::number)
    return num_;
  else
    UNEXP_CONVERSION("(?)", "number");
}

template<>
inline
bool Token::get() const{
  if(type_ == Type::boolean)
    return b_;
  else
    UNEXP_CONVERSION("(?)", "boolean");
}

template<>
inline
char Token::get() const{
  if(type_ == Type::character)
    return c_;
  else
    UNEXP_CONVERSION("(?)", "char");
}

template<>
inline
Token::Notation Token::get() const{
  if(type_ == Type::notation)
    return not_;
  else
    UNEXP_CONVERSION("(?)", "notation");
}

#endif // TOKEN_I_HH
