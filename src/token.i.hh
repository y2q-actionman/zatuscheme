#ifndef TOKEN_I_HH
#define TOKEN_I_HH

#ifndef TOKEN_HH
#error "Please include via parent file"
#endif

#include <utility>

inline
Token::Token(const std::string& s, Type t)
  : type_(t)
{
  new (&this->str_) std::string(s);
}

inline
Token::Token(std::string&& s, Type t)
  : type_(t)
{
  new (&this->str_) std::string(std::move(s));
}

inline
Token::Token(const Number& n)
  : type_(Type::number)
{
  new (&this->num_) Number(n);
}

inline
Token::Token(Number&& n)
  : type_(Type::number)
{
  new (&this->num_) Number(std::move(n));
}

inline
Token::Token(bool b)
  : type_(Type::boolean)
{
  b_ = b;
}

inline
Token::Token(Notation n)
  : type_(Type::notation)
{
  new(&this->not_) Notation(n);
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

inline
std::string Token::str() const{
  switch(type_){
  case Type::identifier:
  case Type::character:
  case Type::string:
    return str_;
  default:
    return "";
  }
}

inline
Number Token::number() const{
  return (type_ == Type::number) ? num_ : Number{};
}

inline
bool Token::boolean() const{
  return (type_ == Type::boolean) ? b_ : false;
}

inline
Token::Notation Token::notation() const{
  return (type_ == Type::notation) ? not_ : Notation::unknown;
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
