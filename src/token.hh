#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>
#include <istream>

#include "number.hh"
#include "util.hh"

class Token {
public:
  enum class Type {
    uninitialized = 0,
      identifier, boolean, number,
      character, string, notation
      };

  enum class Notation {
      l_paren, r_paren, vector_paren,
      quote, quasiquote, comma, comma_at, dot,
      l_bracket, r_bracket,
      l_brace, r_brace,
      bar
      };


  constexpr Token()
    : type_(Type::uninitialized){}

  // TODO: move these constructors into private
  Token(const std::string&, Type);
  Token(std::string&&, Type);
  explicit Token(const Number&);
  explicit Token(Number&&);
  explicit constexpr Token(bool);
  explicit constexpr Token(char);
  explicit constexpr Token(Notation);

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
    Number num_;
    bool b_;
    char c_;
    Notation not_;
  };

  template<typename T> void init_from_other(T other);
  template<typename T> Token& assign_from_other(T other);
};

Token tokenize(std::istream&);

const char* stringify(Token::Notation);
const char* stringify(Token::Type);

#include "token.i.hh"

#endif // TOKEN_HH
