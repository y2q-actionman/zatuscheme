#include <cstdio>
#include <utility>
#include "reader.hh"
#include "lisp_ptr.hh"
#include "token.hh"
#include "number.hh"
#include "cons.hh"
#include "symbol.hh"

using namespace std;

namespace {

Lisp_ptr read_la(FILE*, Token&&);

Lisp_ptr read_list(FILE* f){
  Token t = tokenize(f);

  // first check
  if(!t){
    return Lisp_ptr{};
  }else if(t.type() == Token::Type::notation){
    auto n = t.get<Token::Notation>();
    if(n == Token::Notation::r_paren){ // empty list
      return Cons::NIL;
    }else if(n == Token::Notation::dot){ // error: dotted list has no car.
      return {};
    }
  }

  // main loop
  Cons* c = new Cons();
  const Lisp_ptr head{c};

  while(1){
    Lisp_ptr datum{read_la(f, move(t))};
    if(!datum){
      return Lisp_ptr{};
    }

    c->rplaca(datum);
    
    // check next token
    t = tokenize(f);
    if(!t){
      // TODO: free cons's contents (here or in free_cons_list())
      free_cons_list(head);
      return Lisp_ptr{};
    }else if(t.type() == Token::Type::notation){
      auto n = t.get<Token::Notation>();
      if(n == Token::Notation::r_paren){ // proper list
        c->rplacd(Cons::NIL);
        goto end;
      }else if(n == Token::Notation::dot){ // dotted list
        c->rplacd(read(f));
        t = tokenize(f);
        if(t.type() != Token::Type::notation
           || t.get<Token::Notation>() != Token::Notation::r_paren){
          return Lisp_ptr{}; // error: dotted list has two or more cdrs.
        }
        goto end;
      }
    }
    
    // readying next cons
    Cons* new_c = new Cons();
    c->rplacd(Lisp_ptr{new_c});
    c = new_c;
  }

 end:
  // if(c->cdr() == undef) ...

  return head;
}

Lisp_ptr read_vector(FILE* f){
  Vector* v = new Vector();

  while(1){
    auto t = tokenize(f);
    if(!t){
      // TODO: free vector's contents
      delete v;
      return Lisp_ptr{};
    }else if((t.type() == Token::Type::notation)
             && (t.get<Token::Notation>()
                 == Token::Notation::r_paren)){
      return Lisp_ptr{v};
    }else{
      Lisp_ptr datum{read_la(f, move(t))};
      if(!datum){
        return Lisp_ptr{};
      }

      v->emplace_back(datum);
    }
  }
}

Lisp_ptr read_abbrev(const char* name, FILE* f){
  Lisp_ptr first{intern(VM.symtable, name)};
  Lisp_ptr second{read(f)};

  return Lisp_ptr{new Cons{first, Lisp_ptr{new Cons{second, Cons::NIL}}}};
}

Lisp_ptr read_la(FILE* f, Token&& tok){
  switch(tok.type()){
    // simple datum
  case Token::Type::boolean:
    return Lisp_ptr(tok.move<bool>());

  case Token::Type::number:
    return Lisp_ptr(new Number(tok.move<Number>()));

  case Token::Type::character:
    return Lisp_ptr(tok.move<char>());

  case Token::Type::string:
    return Lisp_ptr(new String(tok.move<string>()));

  case Token::Type::identifier:
    return Lisp_ptr{intern(VM.symtable, tok.move<string>())};

    // compound datum
  case Token::Type::notation:
    switch(auto n = tok.move<Token::Notation>()){

    case Token::Notation::l_paren: // list
      return read_list(f);

    case Token::Notation::vector_paren: // vector
      return read_vector(f);

      // abbrev prefix
    case Token::Notation::quote:
      return read_abbrev("quote", f);

    case Token::Notation::quasiquote:
      return read_abbrev("quasiquote", f);

    case Token::Notation::comma:
      return read_abbrev("unquote", f);

    case Token::Notation::comma_at:
      return read_abbrev("unquote-splicing", f);
      
    case Token::Notation::l_bracket:
    case Token::Notation::l_brace:
      fprintf(stderr, "reader error: not supported notation! (type=%s)\n",
              stringify(n));
      return Lisp_ptr{};

    case Token::Notation::r_paren:
    case Token::Notation::r_bracket:
    case Token::Notation::r_brace:
      fprintf(stderr, "reader error: closing notation appeared alone! (type=%s)\n",
              stringify(n));
      return Lisp_ptr{};

    case Token::Notation::dot:
    case Token::Notation::bar:
    default:
      fprintf(stderr, "reader error: unexpected notation was passed! (type=%s)\n",
              stringify(n));
      return Lisp_ptr{};
    }

  case Token::Type::uninitialized:
  default:
    fprintf(stderr, "reader error: unknown token was passed! (type=%s)\n",
            stringify(tok.type()));
    return Lisp_ptr{};
  }
}

} // namespace

Lisp_ptr read(FILE* f){
  return read_la(f, tokenize(f));
}
