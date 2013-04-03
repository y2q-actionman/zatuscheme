#ifndef TOKEN_HH
#define TOKEN_HH

#include <string>
#include <iosfwd>

#include "number.hh"
#include "decl.hh"
#include "util.hh"

class Token {
public:
  enum class Type {
    uninitialized = 0,
      identifier, boolean, number,
      integer, real, complex,
      character, string, notation
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
    : type_(Type::uninitialized),
      ex_(Exactness::unspecified){}

  // TODO: move these constructors into private
  Token(const std::string&, Type);
  Token(std::string&&, Type);
  explicit Token(const Number&);
  explicit Token(Number&&);
  explicit constexpr Token(bool);
  explicit constexpr Token(char);
  explicit constexpr Token(Notation);
  // numerics
  constexpr Token(int, Exactness);
  constexpr Token(double, Exactness);
  Token(const Complex&, Exactness);
  Token(Complex&&, Exactness);

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

  // numeric interface
  template <typename T> T coerce() const;
  
  Exactness exactness() const
  { return ex_; }

private:
  Type type_;
  union {
    std::string str_;
    Number num_;
    bool b_;
    int i_;
    double d_;
    Complex z_;
    char c_;
    Notation not_;
  };

  // numeric flags
  Exactness ex_;

  template<typename T> void init_from_other(T other);
  template<typename T> Token& assign_from_other(T other);
};

Token tokenize(std::istream&);

// 'radix == 0' means 10 or the specified value of prefix.
Token tokenize_number(std::istream&, int radix = 0);

const char* stringify(Token::Notation);
const char* stringify(Token::Type);

#include "token.i.hh"

#endif // TOKEN_HH
