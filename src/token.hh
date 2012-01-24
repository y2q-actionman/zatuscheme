#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>
#include <iosfwd>
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


  Token()
    : type_(Type::uninitialized){}

  Token(const std::string&, Type);
  Token(std::string&&, Type);
  explicit Token(const Number&);
  explicit Token(Number&&);
  explicit Token(bool);
  explicit Token(char);
  explicit Token(Notation);

  Token(const Token&);
  Token(Token&&);

  ~Token();

  Token& operator=(const Token&);
  Token& operator=(Token&&);
  

  Type type() const
  { return type_; }

  template <typename T>
  T get() const;

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

Token tokenize(std::istream&);

constexpr bool operator==(Token::Type, Token::Type);
constexpr bool operator!=(Token::Type, Token::Type);

void describe(std::ostream&, Token::Type);
void describe(std::ostream&, Token::Notation);
void describe(std::ostream&, const Token&);

#include "token.i.hh"

#endif // TOKEN_HH
