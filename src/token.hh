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
      integer, rational, real, complex,
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
    : type_(Type::uninitialized),
      ex_(Exactness::unspecified){}

  Token(const std::string&, Type);
  Token(std::string&&, Type);
  explicit constexpr Token(Notation);
  // numerics
  constexpr Token(int, Exactness);
  constexpr Token(double, Exactness);
  Token(const Rational&, Exactness);
  Token(Rational&&, Exactness);
  Token(const Complex&, Exactness);
  Token(Complex&&, Exactness);
  //
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

  // numeric interface
  template <typename T> T coerce() const;
  
  Exactness exactness() const
  { return ex_; }

private:
  Type type_;
  union {
    std::string str_;
    int i_;
    Rational q_;
    double d_;
    Complex z_;
    Notation not_;
    Lisp_ptr lisp_value_;
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
