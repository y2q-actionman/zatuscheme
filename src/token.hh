#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>
#include <iosfwd>

#include "decl.hh"
#include "util.hh"
#include "lisp_ptr.hh"

enum class Notation {
  l_paren, r_paren, vector_paren,
    quote, quasiquote, comma, comma_at, dot,
    l_bracket, r_bracket,
    l_brace, r_brace,
    bar
    };


class Token {
public:
  enum class Type {
    uninitialized = 0, notation, lisp_ptr
      };

  constexpr Token()
    : type_(Type::uninitialized){}
  explicit constexpr Token(Notation);
  explicit constexpr Token(Lisp_ptr);

  Token(const Token&) = default;
  Token(Token&&) = default;

  ~Token() = default;

  Token& operator=(const Token&) = default;
  Token& operator=(Token&&) = default;
  

  Type type() const
  { return type_; }

  template <typename T>
  T get() const;

  explicit operator bool() const
  { return type() != Type::uninitialized; }

private:
  Type type_;
  union {
    Notation not_;
    Lisp_ptr lisp_value_;
  };
};

Token tokenize(std::istream&);

// 'radix == 0' means 10 or the specified value of prefix.
Lisp_ptr parse_number(std::istream&, int radix = 0);

const char* stringify(Notation);
const char* stringify(Token::Type);

#include "token.i.hh"

#endif // TOKEN_HH
