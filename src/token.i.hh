#ifndef TOKEN_I_HH
#define TOKEN_I_HH

#ifndef TOKEN_HH
#error "Please include via parent file"
#endif

#include <cassert>

// Type mapping
template<>
struct to_type<Token::Type, Token::Type::identifier>{
  typedef std::string type;
};

template<>
struct to_type<Token::Type, Token::Type::notation>{
  typedef Token::Notation type;
};

template<>
struct to_type<Token::Type, Token::Type::lisp_ptr>{
  typedef Lisp_ptr type;
};


// std::string -> Token::Type is ambigious
template<>
inline constexpr
Token::Type to_tag<Token::Type, std::string>() = delete;

template<>
inline constexpr
Token::Type to_tag<Token::Type, Token::Notation>(){
  return Token::Type::notation;
}

template<>
inline constexpr
Token::Type to_tag<Token::Type, Lisp_ptr>(){
  return Token::Type::lisp_ptr;
}


// Token definitions
inline
Token::Token(const std::string& s, Type t)
  : type_(t), str_(s){}

inline
Token::Token(std::string&& s, Type t)
  : type_(t), str_(std::move(s)){}

inline constexpr
Token::Token(Notation n)
  : type_(Type::notation), not_(n){}

inline constexpr
Token::Token(Lisp_ptr p)
  : type_(Type::lisp_ptr), lisp_value_(p){}

template<>
inline
const std::string& Token::get<std::string>() const{
  assert(type_ == Type::identifier);
  return str_;
}

template<>
inline
Token::Notation Token::get<Token::Notation>() const{
  assert(type_ == Type::notation);
  return not_;
}

template<>
inline
const Lisp_ptr& Token::get<Lisp_ptr>() const{
  assert(type_ == Type::lisp_ptr);
  return lisp_value_;
}


template<>
inline
std::string&& Token::move<std::string>(){
  assert(type_ == Type::identifier);
  return std::move(str_);
}

template<>
inline
Token::Notation Token::move<Token::Notation>(){
  assert(type_ == Type::notation);
  return not_;
}

template<>
inline
Lisp_ptr&& Token::move<Lisp_ptr>(){
  assert(type_ == Type::lisp_ptr);
  return std::move(lisp_value_);
}

#endif // TOKEN_I_HH
