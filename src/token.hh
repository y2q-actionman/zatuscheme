#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>
#include <iosfwd>
#include "number.hh"

class Token {
public:
  enum class Type {
    uninitialized = -1,
      identifier = 0, boolean, number,
      character, string,
      l_paren, r_paren, vector_paren,
      quote, backquote, comma, comma_at, dot,
      reserved
      };

private:
  Type type_;
  union {
    std::string str_;
    Number num_;
    bool b_;
  };

public:
  Token()
    : type_(Type::uninitialized){}

  Token(Type, const std::string&);
  Token(Type, std::string&&);
  Token(Type, const Number&);
  Token(Type, Number&&);
  Token(Type, bool);

  Token(const Token&);
  Token(Token&&);

  ~Token();

  Token& operator=(const Token&);
  Token& operator=(Token&&);
  

  Type type() const
  { return type_; }

  const std::string& str() const
  { return str_; }

  const Number& number() const
  { return num_; }

  bool boolean() const
  { return b_; }

  bool is_syntactic_keyword() const;
  bool is_expression_keyword() const;
};

Token tokenize(std::istream&);


constexpr bool operator==(Token::Type, Token::Type);
constexpr bool operator!=(Token::Type, Token::Type);

#include "token.i.hh"

#endif // TOKEN_HH
