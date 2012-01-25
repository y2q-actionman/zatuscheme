#include <istream>

#include "reader.hh"
#include "lisp_ptr.hh"
#include "token.hh"
#include "number.hh"
#include "keyword.hh"
#include "cons.hh"

using namespace std;

namespace {

Lisp_ptr read_list(istream& i){
  // stub
  (void)i;
  return Lisp_ptr{};
}

Lisp_ptr read_vector(istream& i){
  // stub
  (void)i;
  return Lisp_ptr{};
}

} // namespace

Lisp_ptr read(istream& i){
  Token tok = tokenize(i);

  switch(tok.type()){
    // simple datum
  case Token::Type::boolean:
    return Lisp_ptr(tok.get<bool>());

  case Token::Type::number:
    return Lisp_ptr(new Long_ptr{new Number(tok.get<Number>())});

  case Token::Type::character:
    return Lisp_ptr(tok.get<char>());

  case Token::Type::string:
    return Lisp_ptr{}; // not implemented

  case Token::Type::identifier:
    return Lisp_ptr{}; // not implemented

    // compound datum
  case Token::Type::notation:
    switch(tok.get<Token::Notation>()){

    case Token::Notation::l_paren: // list
      return read_list(i);

    case Token::Notation::vector_paren: // vector
      return read_vector(i);

      // abbrev prefix
    case Token::Notation::quote:
      return cons(Lisp_ptr{Keyword::quote}, read(i));

    case Token::Notation::quasiquote:
      return cons(Lisp_ptr{Keyword::quasiquote}, read(i));

    case Token::Notation::comma:
      return cons(Lisp_ptr{Keyword::unquote}, read(i));

    case Token::Notation::comma_at:
      return cons(Lisp_ptr{Keyword::unquote_splicing}, read(i));
      
    default:
      return Lisp_ptr{};
    }

  default:
    return Lisp_ptr{};
  }
}
