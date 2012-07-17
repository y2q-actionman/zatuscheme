#ifndef TOKEN_I_HH
#define TOKEN_I_HH

#ifndef TOKEN_HH
#error "Please include via parent file"
#endif

#include <cassert>
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
const std::string& Token::get() const{
  assert(type_ == Type::identifier || type_ == Type::string);
  return str_;
}

template<>
inline
const Number& Token::get() const{
  assert(type_ == Type::number);
  return num_;
}

template<>
inline
const bool& Token::get() const{
  assert(type_ == Type::boolean);
  return b_;
}

template<>
inline
const char& Token::get() const{
  assert(type_ == Type::character);
  return c_;
}

template<>
inline
const Token::Notation& Token::get() const{
  assert(type_ == Type::notation);
  return not_;
}

#endif // TOKEN_I_HH
