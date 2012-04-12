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

Lisp_ptr read_la(SymTable&, FILE*, const Token&);

Lisp_ptr read_list(SymTable& sym, FILE* f){
  Token t = tokenize(f);

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
  Cons* c = new Cons{Lisp_ptr{}, Lisp_ptr{}};
  const Lisp_ptr head{c};

  while(1){
    Lisp_ptr datum{read_la(sym, f, t)};
    if(!datum){
      return Lisp_ptr{};
    }

    c->rplaca(datum);
    
    // check next token
    t = tokenize(f);
    if(!t){
      // in future, cleanup working data.
      return Lisp_ptr{};
    }else if(t.type() == Token::Type::notation){
      switch(t.get<Token::Notation>()){
      case Token::Notation::r_paren: // proper list
        c->rplacd(Cons::NIL);
        goto end;
      case Token::Notation::dot:     // dotted list
        c->rplacd(read(sym, f));
        t = tokenize(f);
        if(t.type() != Token::Type::notation
           || t.get<Token::Notation>() != Token::Notation::r_paren){
          return Lisp_ptr{}; // error: dotted list has two or more cdrs.
        }
        goto end;
      default:
        break;
      }
    }
    
    // readying next cons
    Cons* new_c = new Cons{Lisp_ptr{}, Lisp_ptr{}};
    c->rplacd(Lisp_ptr{new_c});
    c = new_c;
  }

 end:
  // if(c->cdr() == undef) ...

  return head;
}

Lisp_ptr read_vector(SymTable& sym, FILE* f){
  Vector* v = new Vector();

  while(1){
    auto t = tokenize(f);
    if(!t){
      // in future, cleanup working data.
      return Lisp_ptr{};
    }else if((t.type() == Token::Type::notation)
             && (t.get<Token::Notation>()
                 == Token::Notation::r_paren)){
      goto end;
    }else{
      Lisp_ptr datum{read_la(sym, f, t)};
      if(!datum){
        return Lisp_ptr{};
      }

      v->emplace_back(datum);
    }
  }

 end:
  return Lisp_ptr{new Long_ptr{v}};
}

Lisp_ptr read_abbrev(SymTable& sym, Keyword k, FILE* f){
  Lisp_ptr first{sym.intern(stringify(k))};
  Lisp_ptr second{read(sym, f)};

  return Lisp_ptr{new Cons{first, Lisp_ptr{new Cons{second}}}};
}

Lisp_ptr read_la(SymTable& sym, FILE* f, const Token& looked_tok){
  auto& tok = (looked_tok) ? looked_tok : tokenize(f);

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
      return read_list(sym, f);

    case Token::Notation::vector_paren: // vector
      return read_vector(sym, f);

      // abbrev prefix
    case Token::Notation::quote:
      return read_abbrev(sym, Keyword::quote, f);

    case Token::Notation::quasiquote:
      return read_abbrev(sym, Keyword::quasiquote, f);

    case Token::Notation::comma:
      return read_abbrev(sym, Keyword::unquote, f);

    case Token::Notation::comma_at:
      return read_abbrev(sym, Keyword::unquote_splicing, f);
      
    default:
      return Lisp_ptr{};
    }

  default:
    return Lisp_ptr{};
  }
}

} // namespace

Lisp_ptr read(SymTable& sym, FILE* f){
  return read_la(sym, f, Token{});
}
