#include <utility>
#include <istream>

#include "reader.hh"
#include "vm.hh"
#include "util.hh"
#include "lisp_ptr.hh"
#include "token.hh"
#include "number.hh"
#include "cons.hh"
#include "cons_util.hh"
#include "symbol.hh"

using namespace std;

namespace {

Lisp_ptr read_la(istream& f, Token&&);

Lisp_ptr read_list(istream& f){
  Token t = tokenize(f);

  // first check
  if(!t){
    return Lisp_ptr{};
  }else if(t.type() == Token::Type::notation){
    auto n = t.get<Token::Notation>();
    if(n == Token::Notation::r_paren){ // empty list
      return Cons::NIL;
    }else if(n == Token::Notation::dot){
      throw zs_error("reader error: dotted list has no car.\n");
    }
  }

  // main loop
  GrowList gl;

  while(1){
    Lisp_ptr datum{read_la(f, move(t))};
    if(!datum){
      return Lisp_ptr{};
    }

    gl.push(datum);
    
    // check next token
    t = tokenize(f);
    if(!t){
      return Lisp_ptr{};
    }else if(t.type() == Token::Type::notation){
      auto n = t.get<Token::Notation>();
      if(n == Token::Notation::r_paren){ // proper list
        return gl.extract();
      }else if(n == Token::Notation::dot){ // dotted list
        auto ret = gl.extract_with_tail(read(f));
        t = tokenize(f);
        if(t.type() != Token::Type::notation
           || t.get<Token::Notation>() != Token::Notation::r_paren){
          throw zs_error("reader error: dotted list has two or more cdrs.\n");
        }
        return ret;
      }
    }
  }

  // if(c->cdr() == undef) ...
  return {};
}

Lisp_ptr read_vector(istream& f){
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

Lisp_ptr read_abbrev(const char* name, istream& f){
  Lisp_ptr first{intern(vm.symtable(), name)};
  Lisp_ptr second{read(f)};

  return make_cons_list({first, second});
}

Lisp_ptr read_la(istream& f, Token&& tok){
  switch(tok.type()){
    // simple datum
  case Token::Type::boolean:
    return Lisp_ptr(tok.move<bool>());

  case Token::Type::number:
    return Lisp_ptr(new Number(tok.move<Number>()));

  case Token::Type::character:
    return (tok.get<char>() == EOF) ? Lisp_ptr{} : Lisp_ptr{tok.move<char>()};

  case Token::Type::string:
    return Lisp_ptr(new String(tok.move<string>()));

  case Token::Type::identifier:
    return Lisp_ptr{intern(vm.symtable(), tok.move<string>())};

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
      throw make_zs_error("reader error: not supported notation! (type=%s)\n",
                          stringify(n));

    case Token::Notation::r_paren:
    case Token::Notation::r_bracket:
    case Token::Notation::r_brace:
      throw make_zs_error("reader error: closing notation appeared alone! (type=%s)\n",
                          stringify(n));

    case Token::Notation::dot:
    case Token::Notation::bar:
    default:
      throw make_zs_error("reader error: unexpected notation was passed! (type=%s)\n",
                          stringify(n));
    }

  case Token::Type::uninitialized:
  default:
    throw make_zs_error("reader error: unknown token was passed! (type=%s)\n",
                        stringify(tok.type()));
  }
}

} // namespace

Lisp_ptr read(istream& f){
  return read_la(f, tokenize(f));
}
