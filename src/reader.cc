#include <istream>

#include "reader.hh"
#include "lisp_ptr.hh"
#include "token.hh"
#include "number.hh"
#include "cons.hh"
#include "symbol.hh"
#include "keyword.hh"
#include "symtable.hh"

using namespace std;

namespace {

Lisp_ptr read_la(SymTable&, istream&, const Token&);

Lisp_ptr read_list(SymTable& sym, istream& i){
  Token t = tokenize(i);

  // first check
  if(!t){
    return Lisp_ptr{};
  }else if(t.type() == Token::Type::notation){
    switch(t.get<Token::Notation>()){
    case Token::Notation::r_paren: // empty list
      return Cons::NIL;
    case Token::Notation::dot:     // error: dotted list has no car.
      return Lisp_ptr{};
    default:
      break;
    }
  }

  // main loop
  const Lisp_ptr head{new Cons{Lisp_ptr{}, Lisp_ptr{}}};
  Lisp_ptr last = head;

  while(1){
    Lisp_ptr datum{read_la(sym, i, t)};
    if(!datum){
      return Lisp_ptr{};
    }

    last.get<Cons*>()->rplaca(datum);
    
    // check next token
    t = tokenize(i);
    if(!t){
      // in future, cleanup working data.
      return Lisp_ptr{};
    }else if(t.type() == Token::Type::notation){
      switch(t.get<Token::Notation>()){
      case Token::Notation::r_paren: // proper list
        last.get<Cons*>()->rplacd(Cons::NIL);
        goto end;
      case Token::Notation::dot:     // dotted list
        last.get<Cons*>()->rplacd(read(sym, i));
        goto end;
      default:
        break;
      }
    }
    
    // readying next cons
    last.get<Cons*>()->rplacd(Lisp_ptr{new Cons{Lisp_ptr{}, Lisp_ptr{}}});
    last = last.get<Cons*>()->cdr();
  }

 end:
  // if(last.get<Cons*>()->cdr() == undef) ...

  return head;
}

Lisp_ptr read_vector(SymTable& sym, istream& i){
  Vector* v = new Vector();

  while(1){
    auto t = tokenize(i);
    if(!t){
      // in future, cleanup working data.
      return Lisp_ptr{};
    }else if((t.type() == Token::Type::notation)
             && (t.get<Token::Notation>()
                 == Token::Notation::r_paren)){
      goto end;
    }else{
      Lisp_ptr datum{read_la(sym, i, t)};
      if(!datum){
        return Lisp_ptr{};
      }

      v->emplace_back(datum);
    }
  }

 end:
  return Lisp_ptr{new Long_ptr{v}};
}

Lisp_ptr read_abbrev(SymTable& sym, Keyword k, istream& i){
  Lisp_ptr first{sym.intern(stringify(k))};
  Lisp_ptr second{read(sym, i)};

  return Lisp_ptr{new Cons{first, Lisp_ptr{new Cons{second}}}};
}

Lisp_ptr read_la(SymTable& sym, istream& i, const Token& looked_tok){
  auto& tok = (looked_tok) ? looked_tok : tokenize(i);

  switch(tok.type()){
    // simple datum
  case Token::Type::boolean:
    return Lisp_ptr(tok.get<bool>());

  case Token::Type::number:
    return Lisp_ptr(new Long_ptr{new Number(tok.get<Number>())});

  case Token::Type::character:
    return Lisp_ptr(tok.get<char>());

  case Token::Type::string:
    return Lisp_ptr(new Long_ptr{new String(tok.get<string>())});

  case Token::Type::identifier:
    return Lisp_ptr{sym.intern(tok.get<string>())};

    // compound datum
  case Token::Type::notation:
    switch(tok.get<Token::Notation>()){

    case Token::Notation::l_paren: // list
      return read_list(sym, i);

    case Token::Notation::vector_paren: // vector
      return read_vector(sym, i);

      // abbrev prefix
    case Token::Notation::quote:
      return read_abbrev(sym, Keyword::quote, i);

    case Token::Notation::quasiquote:
      return read_abbrev(sym, Keyword::quasiquote, i);

    case Token::Notation::comma:
      return read_abbrev(sym, Keyword::unquote, i);

    case Token::Notation::comma_at:
      return read_abbrev(sym, Keyword::unquote_splicing, i);
      
    default:
      return Lisp_ptr{};
    }

  default:
    return Lisp_ptr{};
  }
}

} // namespace

Lisp_ptr read(SymTable& sym, istream& i){
  return read_la(sym, i, Token{});
}
