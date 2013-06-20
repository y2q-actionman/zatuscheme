#include "describe.hh"
#include <ostream>

using namespace std;

std::ostream& operator<<(std::ostream& o, Ptr_tag t){
  return (o << stringify(t));
}

// std::ostream& operator<<(std::ostream& o, Lisp_ptr p){
//   return (o << "[" << p.tag() << "] " << p.get<void*>());
// }

std::ostream& operator<<(std::ostream& o, const ProcInfo& info){
  return (o << "[required_args=" << info.required_args << ", max_args=" << info.max_args << "]");
}

std::ostream& operator<<(std::ostream& o, proc_flag::Variadic v){
  return (o << boolalpha << static_cast<bool>(v) << noboolalpha);
}

std::ostream& operator<<(std::ostream& o, Token::Type t){
  return (o << stringify(t));
}

std::ostream& operator<<(std::ostream& o, Token::Notation n){
  return (o << stringify(n));
}

std::ostream& operator<<(std::ostream& o, const Token& tok){
  const auto t = tok.type();

  o << "Token: " << t << "(";
  switch(t){
  case Token::Type::uninitialized:
    break;
  case Token::Type::identifier:
    o << tok.get<string>();
    break;
  case Token::Type::notation:
    o << tok.get<Token::Notation>();
    break;
  case Token::Type::lisp_ptr:
    o << tok.get<Lisp_ptr>();
    break;
  default:
    UNEXP_DEFAULT();
  }
  o << ')';

  return o;
}
