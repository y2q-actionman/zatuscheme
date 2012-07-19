#ifndef TOKEN_I_HH
#define TOKEN_I_HH

#ifndef TOKEN_HH
#error "Please include via parent file"
#endif

#include <cassert>
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
const std::string& Token::get<std::string>() const{
  assert(type_ == Type::identifier || type_ == Type::string);
  return str_;
}

template<>
inline
const Number& Token::get<Number>() const{
  assert(type_ == Type::number);
  return num_;
}

template<>
inline
bool Token::get<bool>() const{
  assert(type_ == Type::boolean);
  return b_;
}

template<>
inline
char Token::get<char>() const{
  assert(type_ == Type::character);
  return c_;
}

template<>
inline
Token::Notation Token::get<Token::Notation>() const{
  assert(type_ == Type::notation);
  return not_;
}


template<>
inline
std::string&& Token::move<std::string>(){
  assert(type_ == Type::identifier || type_ == Type::string);
  return std::move(str_);
}

template<>
inline
Number&& Token::move<Number>(){
  assert(type_ == Type::number);
  return std::move(num_);
}

template<>
inline
bool Token::move<bool>(){
  assert(type_ == Type::boolean);
  return b_;
}

template<>
inline
char Token::move<char>(){
  assert(type_ == Type::character);
  return c_;
}

template<>
inline
Token::Notation Token::move<Token::Notation>(){
  assert(type_ == Type::notation);
  return not_;
}

#endif // TOKEN_I_HH
