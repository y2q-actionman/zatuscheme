#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>
#include <iosfwd>

#include "decl.hh"
#include "util.hh"
#include "rational.hh"
#include "lisp_ptr.hh"

class Token {
public:
  enum class Type {
    uninitialized = 0,
      identifier,
      notation,
      lisp_ptr
      };

  enum class Notation {
      l_paren, r_paren, vector_paren,
      quote, quasiquote, comma, comma_at, dot,
      l_bracket, r_bracket,
      l_brace, r_brace,
      bar
      };

  enum class Exactness{
    unspecified, exact, inexact
      };


  constexpr Token()
    : type_(Type::uninitialized){}

  Token(const std::string&, Type);
  Token(std::string&&, Type);
  explicit constexpr Token(Notation);
  explicit constexpr Token(Lisp_ptr);


  Token(const Token&);
  Token(Token&&);

  ~Token();

  Token& operator=(const Token&);
  Token& operator=(Token&&);
  

  Type type() const
  { return type_; }

  template <typename T>
  typename zs::call_traits<T>::type get() const;

  template <typename T>
  typename zs::call_traits_r<T>::type move();

  explicit operator bool() const
  { return type() != Type::uninitialized; }

private:
  Type type_;
  union {
    std::string str_;
    Notation not_;
    Lisp_ptr lisp_value_;
  };

  template<typename T> void init_from_other(T other);
  template<typename T> Token& assign_from_other(T other);
};

Token tokenize(std::istream&);

// 'radix == 0' means 10 or the specified value of prefix.
Lisp_ptr parse_number(std::istream&, int radix = 0);

const char* stringify(Token::Notation);
const char* stringify(Token::Type);

#include "token.i.hh"

#endif // TOKEN_HH
