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
  new (&this->str_) (s);
}

inline
Token::Token(Type t, std::string&& s)
  : type_(t)
{
  new (&this->str_) (std::forward(s));
}

inline
Token::Token(Type t, const Number& n)
  : type_(t)
{
  new (&this->num_) (n);
}

inline
Token::Token(Type t, Number&& n)
  : type_(t)
{
  new (&this->num_) (std::forward(n));
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
  case Type::unititialized:
    break;

  case Type::boolean:
    this->b_ = other.b_;
    break;

  case Type::Number:
    new (&this->num_) (other.num_);
    break;

  default:
    new (&this->str_) (other.str_);
    break;
  }
}
  
inline
Token::Token(Token&& other)
  : type_(other.type_)
{
  switch(other.type_){
  case Type::unititialized:
    break;

  case Type::boolean:
    this->b_ = other.b_;
    break;

  case Type::Number:
    new (&this->num_) (std::move(other.num_));
    break;

  default:
    new (&this->str_) (std::move(other.str_));
    break;
  }
}

inline
Token::~Token(){
  switch(type_){
  case Type::unititialized:
    break;

  case Type::boolean:
    break;

  case Type::Number:
    num_.~Number();
    break;

  default:
    str_.~string();
    break;
  }
}

inline
Token& Token::operator=(const Token& other){
  this->type_ = other.type_;

  switch(other.type_){
  case Type::unititialized:
    break;

  case Type::boolean:
    this->b_ = other.b_;
    break;

  case Type::Number:
    this->num_ = other.num_;
    break;

  default:
    this->str_ = other.str_;
    break;
  }
}

inline
Token& Token::operator=(Token&& other){
  this->type_ = other.type_;

  switch(other.type_){
  case Type::unititialized:
    break;

  case Type::boolean:
    this->b_ = other.b_;
    break;

  case Type::Number:
    this->num_ = std::move(other.num_);
    break;

  default:
    this->str_ = std::move(other.str_);
    break;
  }
}
  

#endif // TOKEN_I_HH
