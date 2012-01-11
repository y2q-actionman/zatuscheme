#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>
#include <iosfwd>

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
  std::string str_;

public:
  Token()
    : type_(Type::uninitialized), str_(){}

  Token(Type t, std::string s) 
    : type_(t), str_(s){}

  Token(const Token&) = default;
  Token(Token&&) = default;

  ~Token() = default;

  Token& operator=(const Token&) = default;
  Token& operator=(Token&&) = default;
  

  Type type() const
  { return type_; }

  const std::string& str() const
  { return str_; }

  bool is_syntactic_keyword() const;
  bool is_expression_keyword() const;
};

Token tokenize(std::istream&);

#endif // TOKEN_HH
