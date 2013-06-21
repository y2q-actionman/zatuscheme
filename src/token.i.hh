#ifndef TOKEN_I_HH
#define TOKEN_I_HH

#ifndef TOKEN_HH
#error "Please include via parent file"
#endif

#include <cassert>

// Token definitions
inline constexpr
Token::Token(Notation n)
  : type_(Type::notation), not_(n){}

inline constexpr
Token::Token(Lisp_ptr p)
  : type_(Type::lisp_ptr), lisp_value_(p){}


template<>
inline
Token::Notation Token::get<Token::Notation>() const{
  assert(type_ == Type::notation);
  return not_;
}

template<>
inline
Lisp_ptr Token::get<Lisp_ptr>() const{
  assert(type_ == Type::lisp_ptr);
  return lisp_value_;
}

#endif // TOKEN_I_HH
