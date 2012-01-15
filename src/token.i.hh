#ifndef TOKEN_I_HH
#define TOKEN_I_HH

#ifndef TOKEN_HH
#error "Please include via parent file"
#endif

#include <utility>

inline
Token::Token(Type t, const std::string& s)
  : type_(t)
{
  new (&this->str_) std::string(s);
}

inline
Token::Token(Type t, std::string&& s)
  : type_(t)
{
  new (&this->str_) std::string(std::move(s));
}

inline
Token::Token(Type t, const Number& n)
  : type_(t)
{
  new (&this->num_) Number(n);
}

inline
Token::Token(Type t, Number&& n)
  : type_(t)
{
  new (&this->num_) Number(std::move(n));
}

inline
Token::Token(Type t, bool b)
  : type_(t)
{
  b_ = b;
}

inline
Token::Token(const Token& other)
  : type_(other.type_)
{
  switch(other.type_){
  case Type::uninitialized:
    break;

  case Type::boolean:
    this->b_ = other.b_;
    break;

  case Type::number:
    new (&this->num_) Number(other.num_);
    break;

  default:
    new (&this->str_) std::string(other.str_);
    break;
  }
}
  
inline
Token::Token(Token&& other)
  : type_(other.type_)
{
  switch(other.type_){
  case Type::uninitialized:
    break;

  case Type::boolean:
    this->b_ = other.b_;
    break;

  case Type::number:
    new (&this->num_) Number(std::move(other.num_));
    break;

  default:
    new (&this->str_) std::string(std::move(other.str_));
    break;
  }
}

inline
Token::~Token(){
  using namespace std;

  switch(type_){
  case Type::uninitialized:
    break;

  case Type::boolean:
    break;

  case Type::number:
    num_.~Number();
    break;

  default:
    str_.~string();
    break;
  }

  type_ = Type::uninitialized;
}

inline constexpr
bool operator==(Token::Type lhs, Token::Type rhs){
  return static_cast<int>(lhs) == static_cast<int>(rhs);
}

inline constexpr
bool operator!=(Token::Type lhs, Token::Type rhs){
  return static_cast<int>(lhs) != static_cast<int>(rhs);
}

#endif // TOKEN_I_HH
