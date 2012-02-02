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
  new (&this->str_) std::string{std::forward<std::string>(s)};
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
  new (&this->num_) Number{std::forward<Number>(n)};
}

inline constexpr
Token::Token(bool b)
  : type_(Type::boolean), b_(b){}

inline constexpr
Token::Token(char c)
  : type_(Type::character), c_(c){}

inline constexpr
Token::Token(Notation n)
  : type_(Type::notation), not_(n){}

inline
Token::~Token(){
  using namespace std;

  switch(type_){
  case Type::identifier:
  case Type::string:
    str_.~string();
    break;

  case Type::number:
    num_.~Number();
    break;

  case Type::uninitialized:
  case Type::boolean:
  case Type::character:
  case Type::notation:
  default:
    break;
  }

  type_ = Type::uninitialized;
}

template<>
inline
std::string Token::get() const{
  switch(type_){
  case Type::identifier:
  case Type::string:
    return str_;
  default:
    return "";
  }
}

template<>
inline
Number Token::get() const{
  return (type_ == Type::number) ? num_ : Number{};
}

template<>
inline
bool Token::get() const{
  return (type_ == Type::boolean) ? b_ : false;
}

template<>
inline
char Token::get() const{
  return (type_ == Type::character) ? c_ : '\0';
}

template<>
inline
Token::Notation Token::get() const{
  return (type_ == Type::notation) ? not_ : Notation::unknown;
}

#endif // TOKEN_I_HH
