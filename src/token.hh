#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>
#include <cstdio>
#include "number.hh"

class Token {
public:
  enum class Type {
    uninitialized = 0,
      identifier, boolean, number,
      character, string, notation
      };

  enum class Notation {
    unknown = 0,
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
  T get() const;

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
};

Token tokenize(FILE*);

const char* stringify(Token::Notation);
const char* stringify(Token::Type);

void describe(FILE*, Token::Type);
void describe(FILE*, Token::Notation);
void describe(FILE*, const Token&);

#include "token.i.hh"

#endif // TOKEN_HH
