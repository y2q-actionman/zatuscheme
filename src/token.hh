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
  Token() = delete;
  Token(std::istream&);
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

private:
  bool tokenize_identifier(std::istream&);
  bool tokenize_boolean(std::istream&);
  bool tokenize_character(std::istream&);
  bool tokenize_string(std::istream&);
};

#endif // TOKEN_HH
