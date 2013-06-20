#ifndef TOKEN_I_HH
#define TOKEN_I_HH

#ifndef TOKEN_HH
#error "Please include via parent file"
#endif

#include <cassert>

// Type mapping
template<>
struct to_type<Token::Type, Token::Type::identifier>{
  typedef std::string type;
};

template<>
struct to_type<Token::Type, Token::Type::boolean>{
  typedef bool type;
};

template<>
struct to_type<Token::Type, Token::Type::integer>{
  typedef int type;
};

template<>
struct to_type<Token::Type, Token::Type::rational>{
  typedef Rational type;
};

template<>
struct to_type<Token::Type, Token::Type::real>{
  typedef double type;
};

template<>
struct to_type<Token::Type, Token::Type::complex>{
  typedef Complex type;
};

template<>
struct to_type<Token::Type, Token::Type::character>{
  typedef char type;
};

template<>
struct to_type<Token::Type, Token::Type::string>{
  typedef std::string type;
};

template<>
struct to_type<Token::Type, Token::Type::notation>{
  typedef Token::Notation type;
};

template<>
struct to_type<Token::Type, Token::Type::lisp_ptr>{
  typedef Lisp_ptr type;
};


// std::string -> Token::Type is ambigious
template<>
inline constexpr
Token::Type to_tag<Token::Type, std::string>() = delete;

template<>
inline constexpr
Token::Type to_tag<Token::Type, bool>(){
  return Token::Type::boolean;
}

template<>
inline constexpr
Token::Type to_tag<Token::Type, int>(){
  return Token::Type::integer;
}

template<>
inline constexpr
Token::Type to_tag<Token::Type, Rational>(){
  return Token::Type::rational;
}

template<>
inline constexpr
Token::Type to_tag<Token::Type, double>(){
  return Token::Type::real;
}

template<>
inline constexpr
Token::Type to_tag<Token::Type, Complex>(){
  return Token::Type::complex;
}

template<>
inline constexpr
Token::Type to_tag<Token::Type, char>(){
  return Token::Type::character;
}

template<>
inline constexpr
Token::Type to_tag<Token::Type, Token::Notation>(){
  return Token::Type::notation;
}

template<>
inline constexpr
Token::Type to_tag<Token::Type, Lisp_ptr>(){
  return Token::Type::lisp_ptr;
}


// Token definitions
inline
Token::Token(const std::string& s, Type t)
  : type_(t), str_(s),
    ex_(Exactness::unspecified){}

inline
Token::Token(std::string&& s, Type t)
  : type_(t), str_(std::move(s)),
    ex_(Exactness::unspecified){}

inline constexpr
Token::Token(bool b)
  : type_(Type::boolean), b_(b),
    ex_(Exactness::unspecified){}

inline constexpr
Token::Token(int i, Exactness ex)
  : type_(Type::integer), i_(i),
    ex_(ex){}

inline
Token::Token(const Rational& q, Exactness ex)
  : type_(Type::rational), q_(q),
    ex_(ex){}

inline
Token::Token(Rational&& q, Exactness ex)
  : type_(Type::rational), q_(std::move(q)),
    ex_(ex){}

inline constexpr
Token::Token(double d, Exactness ex)
  : type_(Type::real), d_(d),
    ex_(ex){}

inline
Token::Token(const Complex& z, Exactness ex)
  : type_(Type::complex), z_(z),
    ex_(ex){}

inline
Token::Token(Complex&& z, Exactness ex)
  : type_(Type::complex), z_(std::move(z)),
    ex_(ex){}

inline constexpr
Token::Token(char c)
  : type_(Type::character), c_(c),
    ex_(Exactness::unspecified){}

inline constexpr
Token::Token(Notation n)
  : type_(Type::notation), not_(n),
    ex_(Exactness::unspecified){}

inline constexpr
Token::Token(Lisp_ptr p)
  : type_(Type::lisp_ptr), lisp_value_(p),
    ex_(Exactness::unspecified){}

template<>
inline
const std::string& Token::get<std::string>() const{
  assert(type_ == Type::identifier || type_ == Type::string);
  return str_;
}

template<>
inline
bool Token::get<bool>() const{
  assert(type_ == Type::boolean);
  return b_;
}

template<>
inline
int Token::get<int>() const{
  assert(type_ == Type::integer);
  return i_;
}

template<>
inline
const Rational& Token::get<Rational>() const{
  assert(type_ == Type::rational);
  return q_;
}

template<>
inline
double Token::get<double>() const{
  assert(type_ == Type::real);
  return d_;
}

template<>
inline
const Complex& Token::get<Complex>() const{
  assert(type_ == Type::complex);
  return z_;
}

template<>
inline
char Token::get<char>() const{
  assert(type_ == Type::character);
  return c_;
}

template<>
inline
Token::Notation Token::get<Token::Notation>() const{
  assert(type_ == Type::notation);
  return not_;
}

template<>
inline
const Lisp_ptr& Token::get<Lisp_ptr>() const{
  assert(type_ == Type::lisp_ptr);
  return lisp_value_;
}


template<>
inline
std::string&& Token::move<std::string>(){
  assert(type_ == Type::identifier || type_ == Type::string);
  return std::move(str_);
}

template<>
inline
bool Token::move<bool>(){
  assert(type_ == Type::boolean);
  return b_;
}

template<>
inline
char Token::move<char>(){
  assert(type_ == Type::character);
  return c_;
}

template<>
inline
int Token::move<int>(){
  assert(type_ == Type::integer);
  return b_;
}

template<>
inline
Rational&& Token::move<Rational>(){
  assert(type_ == Type::rational);
  return std::move(q_);
}

template<>
inline
double Token::move<double>(){
  assert(type_ == Type::real);
  return d_;
}

template<>
inline
Complex&& Token::move<Complex>(){
  assert(type_ == Type::complex);
  return std::move(z_);
}

template<>
inline
Token::Notation Token::move<Token::Notation>(){
  assert(type_ == Type::notation);
  return not_;
}

template<>
inline
Lisp_ptr&& Token::move<Lisp_ptr>(){
  assert(type_ == Type::lisp_ptr);
  return std::move(lisp_value_);
}

template <> int Token::coerce() const;
template <> Rational Token::coerce() const;
template <> double Token::coerce() const;
template <> Complex Token::coerce() const;

#endif // TOKEN_I_HH
